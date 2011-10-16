-module(echat_server).
-author('Dawid Figiel').
-compile(export_all).

%% TCP options for listening socket.
-define(TCP_OPTIONS, [binary, 
                      {packet, 0}, 
                      {active, false}, 
                      {reuseaddr, true}]).

-include("user.hrl").

%% For clarity functions from utils are imported.
-import(utils,
        [get_config/1, send/2, find_user/2, find_user_by_name/2,
         find_user_by_groups/2, delete_user/2, publiced_users_list/1,
         send_broadcast/3, broadcast_updates/1, broadcast_updates_del/1,
         update_groups/2, update_groups_del/2, update_remote_servers/3,
         get_users_from_remote_servers/1
        ]).

%% @doc
%% This is the entry point for chat server.
%% TODO: Add supervisor and application bahaviour.
%% @end
start(ConfFile)->
    Conf = get_config(ConfFile),
    Port = proplists:get_value(port, Conf), 
    listen(Port,Conf).

%% @doc
%% To allow incoming connections, we need to listen on a TCP port.
%% It starts the client_manager process and gives it a name so the rest
%% of the code can get to it easily.
%% @end
listen(Port,Conf) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    io:format("Conf:~p~n",[Conf]),
    register(client_manager, spawn(fun()-> 
                                           client_manager([],[],Conf) 
                                   end)),
    get_users_from_remote_servers(Conf),
    do_accept(LSocket).

%% @doc
%% Accepting a connection gives us a connection socket with the
%% newly-connected client. Since we want to accept more than one 
%% client, we spawn a new process for each and then wait
%% for another connection on listening socket.
%% @end
do_accept(LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->                    
            spawn(fun() -> handle_client(Socket) end),
            client_manager ! {connect, Socket};
        {error, Reason} ->
            io:format("Socket accept error:~p~n",[Reason])
    end,
    do_accept(LSocket).

%% @doc
%% All the client-socket process needs to do is wait for data and
%% forward it to the client_manager process which decides what to do
%% with it. If the client disconnects, we let client_manager know and
%% then quietly go away.
%% @end
handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            client_manager ! {tcp, Socket, Data},
            handle_client(Socket);
        {error, closed} ->
            client_manager ! {disconnect, Socket} 
    end.

%% @doc
%% This is the main loop of the client_manager process.
%% It maintains the list of users calls the handler for client input
%% and RPC requests.
%% @end
client_manager(Users, Groups, Conf) ->
    receive
        {connect, Socket} ->
            User = #user{socket=Socket, mode=connected},
            NewUsers = [User | Users],
            NewGroups = Groups,
            io:format("Client ~p connected~n", [User#user.socket]),
            send(User#user.socket, authentication_request),
            {NewUsers, NewGroups};         
        {disconnect, Socket} ->
            User = find_user(Socket, Users),
            io:format("Client ~s disconnected~n", [User#user.name]),
            NewUsers = lists:delete(User, Users),
            NewGroups = update_groups_del(Groups, User),
            io:format("Updated groups:~p~n",[NewGroups]),
            broadcast_updates_del(NewUsers),
            update_remote_servers(rm_remote_user, 
                                  Conf, 
                                  User#user{mode=remote, node=node()}),
            {NewUsers, NewGroups}; 
        {rpc, add_remote_user, User} ->
            %% If remote user exists then replace with the new one.
            case lists:keysearch(User#user.name, #user.name, Users) of
                {value, ExistingUser} ->
                    io:format("Update remote user ~s~n", [User#user.name]),
                    NewGroups0 = update_groups_del(Groups, ExistingUser),
                    NewGroups = update_groups(NewGroups0, User),
                    NewUsers = [User | delete_user(ExistingUser, Users)];
                false ->
                    io:format("Add remote user ~s~n", [User#user.name]),
                    NewGroups = update_groups(Groups, User),
                    NewUsers = [User | Users]
            end, 
            timer:sleep(500),
            broadcast_updates(NewUsers),
            {NewUsers, NewGroups}; 
        {rpc, remote_msg, From, To, Str} ->
            io:format("(forwarded)~s -> ~s > ~s~n",[From, To, Str]),
            ToUser = find_user_by_name(To, Users),
            send(ToUser#user.socket, {msg_fwd, From, Str}),
            NewUsers = Users,
            NewGroups = Groups,
            {NewUsers, NewGroups};
        {rpc, rm_remote_user, User} ->
            io:format("Remove remote user ~s~n", [User#user.name]),
            NewGroups = update_groups_del(Groups, User),
            io:format("Updated groups:~p~n",[NewGroups]),
            NewUsers = lists:delete(User, Users),
            broadcast_updates_del(NewUsers),
            {NewUsers, NewGroups}; 
        {rpc, update_remote_user_group, UUser} ->
            io:format("Update remote user ~s~n", [UUser#user.name]),
            User = find_user_by_name(UUser#user.name, Users),
            NewGroups0 = update_groups_del(Groups, User),
            NewGroups = update_groups(NewGroups0, UUser),
            NewUsers = [UUser | delete_user(User, Users)],
            {NewUsers, NewGroups};
        {rpc, sync_users_list, RemoteNode} ->
            io:format("Remote server ~p is asking for users~n", [RemoteNode]),
            lists:foreach(fun(User)->
                          rpc:call(RemoteNode, 
                                   echat_server, 
                                   add_remote_user, 
                                   [User#user{mode = remote, node=node()}])
                          end, Users),
            NewUsers = Users,
            NewGroups = Groups,
            {NewUsers, NewGroups};
        {tcp, Socket, BinData} ->
            {NewUsers, NewGroups} = handle_tcp_data(Socket, 
                                                    BinData, 
                                                    Users, 
                                                    Groups, 
                                                    Conf)
    end,
    client_manager(NewUsers, NewGroups, Conf).

%% @doc
%% Handle TCP Data which is comming from the client tcp socket.
%% @end
handle_tcp_data(Socket, BinData, Users, Groups, Conf) ->
    case binary_to_term(BinData) of
        {msg, From, To, Str} ->      
            ToUser = find_user_by_name(To, Users),
            case ToUser#user.mode of
                active ->
                    io:format("(local)~s -> ~s > ~s~n",[From, To, Str]),
                    send(ToUser#user.socket, {msg_fwd, From, Str});
                remote ->
                    io:format("(forward)~s -> ~s > ~s~n",[From, To, Str]),
                    rpc:call(ToUser#user.node, 
                             echat_server, 
                             remote_msg, [From, To, Str])
            end,
            {Users, Groups};
        {broadcast, UserGroups, Str} ->             
            User = find_user(Socket, Users),
            io:format("~s -> ~p > ~s~n",[User#user.name, UserGroups, Str]),
            send_broadcast(User, 
                           find_user_by_groups(UserGroups, Users), 
                           Str),
            {Users, Groups};
        {login, Pwd, Nick, InitilGroups} ->      
            User = find_user(Socket, Users),
            UUser = User#user{name = Nick, 
                              mode=active, 
                              password = Pwd,  %% TODO: Handle passwords 
                              groups = InitilGroups},
            %% If local user exists then replace with the new one.
            case lists:keysearch(Nick, #user.name, Users) of
                {value, ExistingUser} ->
                    io:format("Update local user ~s~n", [Nick]),
                    NewGroups0 = update_groups_del(Groups, ExistingUser),
                    NewGroups = update_groups(NewGroups0, UUser),
                    NewUsers = [UUser | delete_user(ExistingUser, Users)];
                false ->
                    io:format("Add local user ~s~n", [Nick]),
                    NewGroups = update_groups(Groups, UUser),
                    NewUsers = [UUser | delete_user(User, Users)]
            end,
            send(Socket, 
                 {login_ack,"Welcome on board " ++ Nick ++ "!"}),
            io:format("Client ~p login~n", [UUser#user.name]),
            timer:sleep(500),
            broadcast_updates(NewUsers),
            update_remote_servers(add_remote_user, 
                                  Conf, 
                                  UUser#user{mode=remote, node=node()}),
            {NewUsers, NewGroups};
        {update_my_groups, UserGroups} ->
            User = find_user(Socket, Users),
            UUser = User#user{groups = UserGroups},
            NewGroups0 = update_groups_del(Groups, User),
            NewGroups = update_groups(NewGroups0, UUser),
            update_remote_servers(update_remote_user_group, 
                                  Conf, 
                                  UUser#user{mode=remote, node=node()}),
            NewUsers = [UUser | delete_user(User, Users)],
            {NewUsers, NewGroups};
        {get_buddies, all} ->      
            User = find_user(Socket, Users),
            send(Socket, 
                 {buddies_list,
                  publiced_users_list(delete_user(User, Users))}),
            {Users, Groups};
        {get_groups, all} ->
            GroupsNames = lists:map(fun({G,_})-> G end, Groups),
            send(Socket, {groups_list, GroupsNames}),
            {Users, Groups};
        {get_nicks_in_group, GroupName} ->
            UsersinGroup = find_user_by_groups([GroupName], Users),
            User = find_user(Socket, Users),
            send(Socket, 
                 {buddies_list,
                  publiced_users_list(
                    delete_user(User, UsersinGroup))}),
            {Users, Groups};
        X ->
            io:format("Not handled TCP data:~p",[X]),
            {Users, Groups}
    end.

%% @doc
%% To add remote user, invoked from remote server.
%% @end
add_remote_user(User)->
    client_manager ! {rpc, add_remote_user, User}.

%% @doc
%% To remove remote user, invoked from remote server.
%% @end
rm_remote_user(User)->
    client_manager ! {rpc, rm_remote_user, User}.

%% @doc
%% Forward message from client registerd on different server,
%% invoked from remote server.
%% @end
remote_msg(From, To, Str)->
    client_manager ! {rpc, remote_msg, From, To, Str}.

%% @doc
%% Update groups of remote client, invoked from remote server.
%% @end
update_remote_user_group(User) ->
    client_manager ! {rpc, update_remote_user_group, User}.

%% @doc
%% Get users from remote server when starting up.
%% @end
sync_users_list(RemoteNode) ->
    client_manager ! {rpc, sync_users_list, RemoteNode}.

%%===================================================================
%% For testing
%%===================================================================
start1()->
    start("server1.conf").

start2()->
    start("server2.conf").

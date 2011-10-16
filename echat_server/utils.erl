-module(utils).
-author('Dawid Figiel').
-compile(export_all).

-include("user.hrl").

%% @doc
%% find_user is a utility function to get a user record associated
%% with a particular socket out of the user list.
%% @end
find_user(Socket, Users) ->
    {value, User} = lists:keysearch(Socket, #user.socket, Users),
    User.

%% @doc
%% find_user_by_name is a utility function to get a user record 
%% associated with a name out of the user list.
%% We assume each name is unique.
%% @end
find_user_by_name(Name, Users) ->
    {value, User} = lists:keysearch(Name, #user.name, Users),
    User.

%% @doc
%% find_user_by_groups is a utility function to get a user records 
%% which belongs to given groups.
%% @end
find_user_by_groups(GroupsNames, Users) ->
    lists:foldl(fun(User, Acc) -> 
                        case sets:to_list(sets:intersection(
                                            sets:from_list(GroupsNames),
                                            sets:from_list(User#user.groups))) of
                            [] -> Acc;
                            [_|_] -> [User|Acc]
                        end
                end, [], Users).

%% @doc
%% delete_user returns the user list without the given user.  It
%% deletes the user from the list based on the socket rather than
%% the whole record because the list might hold a different version.
%% @end
delete_user(User, Users) ->
    lists:keydelete(User#user.socket, #user.socket, Users).

%% @doc
%% Prepare users to be sent to client server.
%% Return list of tuples with groups and user names:
%% [{UserGroups, UserName},...]
%% @end
publiced_users_list(Users) ->
    publiced_users_list(Users, []).
publiced_users_list([], PublicList) ->
    PublicList;
publiced_users_list([#user{name = Name, mode = active, groups = Groups} | Users], 
                    PublicList) ->
    publiced_users_list(Users, [{Groups, Name} | PublicList]);
publiced_users_list([#user{name = Name, mode = remote, groups = Groups} | Users], 
                    PublicList) ->
    publiced_users_list(Users, [{Groups, Name} | PublicList]);
publiced_users_list([#user{mode = connected} | Users], PublicList) ->
    publiced_users_list(Users, PublicList).

%% @doc
%% Send message to active users related to local server (through socket)
%% and  those who are connected to romote servers (RPC calls).
%% @end
send_broadcast(From, Users, Str) ->
    lists:foreach(fun(To) ->
                          case To#user.mode of
                              active ->                         
                                  io:format("(local)~s -> ~s > ~s~n",
                                            [From#user.name, 
                                             To#user.name, Str]),
                                  utils:send(To#user.socket,
                                             {msg_fwd, 
                                              From#user.name, Str});
                              remote ->
                                  io:format("(remote)~s -> ~s > ~s~n",
                                            [From#user.name, 
                                             To#user.name, Str]),
                                  rpc:call(To#user.node, 
                                           echat_server, 
                                           remote_msg,
                                           [From#user.name, 
                                            To#user.name, Str]);
                              _ -> ok
                          end
                  end, delete_user(From, Users)).

%% @doc
%% Send updated buddies list to clients connected with local server.
%% If the list is empty nothing is sent.
%% @end
broadcast_updates(NewUsers) ->
    lists:foreach(fun(U) ->
                          case U#user.mode of
                              active ->                         
                                  BuddiesList = publiced_users_list(
                                                  delete_user(U,NewUsers)),
                                  case BuddiesList of
                                      [] -> 
                                          ok;
                                      _ ->
                                          utils:send(U#user.socket,
                                                     {buddies_list, 
                                                      BuddiesList})
                                  end;
                              _ -> 
                                  ok
                          end
                  end, NewUsers).

%% @doc
%% Send updated buddies list to clients connected with local server.
%% Empty is sent too.
%% @end
broadcast_updates_del(NewUsers) ->
    lists:foreach(fun(U) ->
                          case U#user.mode of
                              active ->                             
                                  BuddiesList = publiced_users_list(
                                                  delete_user(U,NewUsers)),
                                  utils:send(U#user.socket,
                                             {buddies_list, BuddiesList});
                              _ -> 
                                  ok
                          end
                  end, NewUsers).

%% @doc
%% Update list of the groups when new client is connected.
%% Print groups with amount of user which belongs to each group.
%% @end
update_groups(Groups, #user{groups = UserGroups}) -> 
    G = lists:foldl(fun(UG, Acc)->
                            case proplists:get_value(UG, Acc) of
                                undefined -> 
                                    [{UG, 1} | Acc];
                                N ->
                                    [{UG, N + 1} | 
                                     proplists:delete(UG, Acc)]
                            end
                    end, Groups, UserGroups),
    io:format("Updated groups:~p~n",[G]),
    G.

%% @doc
%% Update list of the groups when new client is disconnected.
%% @end
update_groups_del(Groups, #user{groups = UserGroups}) -> 
    lists:foldl(fun(UG, Acc)->
                        N = proplists:get_value(UG, Acc),
                        [{UG, N - 1} | proplists:delete(UG, Acc)]
                end, Groups, UserGroups).

%% @doc
%% Generic function to invoke rpc commands with user as an argument.
%% @end
update_remote_servers(F, Conf, User)->
    Nodes = proplists:get_value(nodes, Conf), 
    lists:foreach(fun(Node)->
                          rpc:call(Node, echat_server, F, [User])
                  end, Nodes).

%% @doc
%% Synchronise users list.
%% @end
get_users_from_remote_servers(Conf)->
    Nodes = proplists:get_value(nodes, Conf), 
    lists:foreach(fun(Node)->
                          rpc:call(Node, echat_server, 
                                   sync_users_list, [node()])
                  end, Nodes).

%% @doc
%% Get configuration parameters from file.
%% @end
get_config(ConfigFile) ->
    case file:consult(ConfigFile) of
	{ok, ConfigData} ->
            ConfigData;
	{error, Why} ->
	    exit({error, Why})
    end.

%% @doc
%% Send data through tcp socket.
%% @end
send(Socket, Data)->
    gen_tcp:send(Socket, term_to_binary(Data)).

-module(echat_client).
-author('Dawid Figiel').
-compile(export_all).

%% Record definition for chat state
-record(state, {mode, %% undefined OR private OR broadcast
                recent_buddy,
                recent_groups = [],
                host = "localhost",
                port = 8888,
                password,
                name,
                groups = []
               }).

%% For clarity functions from io_widget are imported.
-import(io_widget, 
	[get_state/1, insert_str/2, set_prompt/2, set_state/2, 
	 set_title/2, set_handler/2, update_state/3, 
         update_buddies/2, update_groups/2]).

%% For clarity functions from utils are imported.
-import(utils,
        [get_config/1, send/2, client_connect/2]).

%% @doc
%% This is the entry point for chat client.
%% TODO: Add supervisor and application bahaviour.
%% @end
start_one_client(Conf) ->
    spawn(fun()-> 
                  connect(proplists:get_value(host, Conf),
                          proplists:get_value(port, Conf),
                          proplists:get_value(password, Conf),
                          proplists:get_value(name, Conf),
                          proplists:get_value(groups, Conf)) 
          end).

%% @doc
%% Start IO_Widget,connect to the server and run the Connection 
%% Manager.
%%
%% <pre>
%% IO_Widget               Connection Manager             Echat Server
%%   |                             |                             |
%%   |-------- Widget(Pid)-------->|                             |
%%   |<------- set state ----------|                             |
%%   |<------- set prompt ---------|                             |
%%   |<------- set handler --------|                             |
%%   |                             |------ connect ------------->|
%%   :                                                           :
%%
%% </pre>
%% @end
connect(Host, Port, Pwd, Nick, Groups) ->
    process_flag(trap_exit, true),
    Widget = io_widget:start(self(), Nick),
    set_state(Widget, Nick),
    set_prompt(Widget, [Nick, " >"]),
    set_handler(Widget, fun parse_command/1),
    {ok, Socket} = try_to_connect(Widget, Host, Port),
    manage_connection(Widget, Socket, #state{host = Host, 
                                             port = Port,
                                             password = Pwd,
                                             name = Nick,
                                             groups = ["General" | Groups],
                                             recent_groups = ["General" | Groups]
                                            }).
%% @doc
%% Try to get TCP connection with the server. 
%% Function return Socket.
%%
%% <pre>
%% Connection Manager                                Echat Server
%%   |                                                     |
%%   |-------------------- connect ----------------------->|
%%   :                    failed                           :
%%   |-------------------- connect ----------------------->|
%%   :                   successed                         :
%%   |<---------------- {ok, Socket} ----------------------|
%%   :                                                     :
%%
%% </pre>
%% @end
try_to_connect(Widget, Host, Port) ->
     case client_connect(Host, Port) of
         {ok, Socket} ->                   
             {ok, Socket};
         _ ->
             insert_str(Widget, 
                        "Failed to connect. Next retry after 5 sconds...\n"),
             timer:sleep(5000),
             try_to_connect(Widget, Host, Port)
     end.
       
%% @doc
%% Handle IO_Widget signals and TCP Data comming from the server.
%% Returns from the Server are handled by function handle_tcp_data.
%%
%% <pre>
%% IO_Widget               Connection Manager                 Echat Server
%%   :                              :                                :
%%   |------------ status --------->|                                |
%%   :                              :                                :
%%   |------ selected_buddy ------->|                                |
%%   :                              :                                :
%%   |------ selected_group ------->|                                |
%%   |                              |------ update_my_groups ------->|
%%   :                              :            handle_tcp_data <---:
%%   |------ get_all_groups ------->|                                |
%%   |                              |------ get_groups ------------->|
%%   :                              :            handle_tcp_data <---:
%%   :                              :                                :
%%   |------ get_all_buddies ------>|                                |
%%   |                              |------ get_buddies ------------>|
%%   :                              :            handle_tcp_data <---:
%%   :                              :                                :
%%   |------ get_nicks_in_group --->|                                |
%%   |                              |------ get_nicks_in_group ----->|
%%   :                              :            handle_tcp_data <---:
%%   :                              :                                :
%%   |------ {Wiget, From, Str} --->|                                |
%%   |                              |------ msg/broadcast ---------->|
%%   :                              :                                :
%%   |                              |<----- tcp_closed --------------|
%%   |<----- update_buddies --------|                                |
%%   |                              |------ connect ---------------->|
%%   :                              :                                :
%%
%% </pre>
%% @end         
manage_connection(Widget, Socket, State) ->
    receive
	{Widget, destroyed} ->
            exit(widgetDestroyed);
	{status, S} ->
	    insert_str(Widget, io_lib:format("~p~n",[S])),
	    manage_connection(Widget, Socket, State);
        {Widget, selected_buddy, NewTo} ->
            manage_connection(Widget, 
                              Socket, 
                              State#state{mode = private, 
                                          recent_buddy = NewTo});
        {Widget, selected_group, NewTo} ->
            RG = State#state.recent_groups,
            NewRG = case lists:member(NewTo,RG) of
                        true ->           
                            insert_str(Widget, 
                                       io_lib:format("Left group:~p\n",
                                                     [NewTo])),
                            RG -- [NewTo];
                         false ->
                            insert_str(Widget, 
                                       io_lib:format("Joined group:~p\n",
                                                     [NewTo])),
                            RG ++ [NewTo]
                    end, 
            insert_str(Widget, io_lib:format("Your groups:~p\n",[NewRG])),
            ok = send(Socket, {update_my_groups, NewRG}),
            manage_connection(Widget, 
                              Socket, 
                              State#state{mode = broadcast, 
                                          recent_groups = NewRG});
        {Widget, get_all_groups} ->
            send(Socket, {get_groups, all}),
	    insert_str(Widget, "Mode: broadcast\n"),
            manage_connection(Widget, 
                              Socket, 
                              State#state{mode = broadcast});
        {Widget, get_all_buddies} ->
	    insert_str(Widget, "Mode: one 2 one\n"),
            ok = send(Socket, {get_buddies, all}),
            manage_connection(Widget, 
                              Socket, 
                              State#state{mode = private});
        {Widget,{get_nicks_in_group, GroupName}}->
	    insert_str(Widget, "Mode: one 2 one\n"),
            ok = send(Socket, {get_nicks_in_group, GroupName}),
            manage_connection(Widget, 
                              Socket, 
                              State#state{mode = private});
        {Widget, From, Str} ->
            case State#state.mode of
                undefined ->
                    insert_str(Widget, 
                               "Press Get Buddies or Channels\n");
                private ->
                    To = State#state.recent_buddy,
                    case To of
                        undefined -> insert_str(Widget, "Select buddy\n");
                        _ ->
                            insert_str(Widget, 
                                       io_lib:format("To ~s >~s~n",
                                                     [To, Str])),
                            send(Socket, {msg, From, To, Str})
                    end;
                broadcast ->
                    To = State#state.recent_groups,
                    case To of
                        [] -> 
                            insert_str(Widget, 
                                       "Select your group (doubleclik)\n");
                        [_|_] ->
                            insert_str(Widget, 
                                       io_lib:format("To:~p >~s~n",
                                                     [To, Str])),
                            send(Socket, {broadcast, To, Str}) 
                    end
            end,
            manage_connection(Widget, Socket, State);                
        {tcp, Socket, BinData} ->
            handle_tcp_data(Widget, Socket, BinData, State),
            manage_connection(Widget, Socket, State);
        {tcp_closed, Socket} ->
            gen_tcp:close(Socket),
	    insert_str(Widget, 
                       "Connection lost. Trying to connect...\n"),
            update_buddies(Widget, []),
            {ok, NewSocket} = try_to_connect(Widget, 
                                             State#state.host, 
                                             State#state.port),
            manage_connection(Widget, NewSocket, State);
        {'EXIT',_Widget, windowDestroyed} ->
            gen_tcp:close(Socket);
        {close, Socket} ->
            exit(serverDied);
        _Other -> %% For debugging
	    %% insert_str(Widget, io_lib:format("Unexpected:~p~n",[Other])),
	    manage_connection(Widget, Socket, State)
     end. 

%% @doc
%% Handle TCP Data comming from the server.
%% Connection Manager == Handle TCP Data
%%
%% <pre>
%% IO_Widget               Handle TCP Data                   Echat Server
%%   :                              :                                :
%%   |                              |<--- authentication_request ----|
%%   |                              |------------ login ------------>|
%%   |                              |<-------- login_ack ------------|
%%   |<------------ Print Welcome --|                                |
%%   :                              :                                :
%%   |                              |<-------- buddies_list ---------|   
%%   |<---- Update Buddies List ----|                                |
%%   |                              |<-------- group_list -----------|   
%%   |<------ Update Group List ----|                                |
%%   :                              :                                :
%%   |                              |<---------- msg_fwd ------------|
%%   |<------ Print Message --------|                                |
%%   :                              :                                :
%%
%% </pre>
%% @end
handle_tcp_data(Widget, Socket, BinData, State) ->
    case binary_to_term(BinData) of
        authentication_request ->
            ok = send(Socket, 
                      {login, 
                       State#state.password, 
                       State#state.name, 
                       State#state.groups});
        {login_ack, Str} ->
            insert_str(Widget, 
                       io_lib:format("~s~n",[Str]));
        {buddies_list, BuddiesList} ->
            update_buddies(Widget, BuddiesList);
        {groups_list, GroupsList} ->
            update_groups(Widget, GroupsList);
        {msg_fwd, From, Str} ->      
            insert_str(Widget, 
                       io_lib:format("From ~s >~s~n",[From, Str]));
        X ->
            io:format("Error:~p",[X])
    end.


% @doc
%% Callback function for IO_Widget, for parsing user's input.
%% @end
parse_command(">" ++ T) -> 
    T;
parse_command([_|T]) -> 
    parse_command(T);
parse_command([]) -> 
    exit("no >").

%%======================================================================
%% For testing
%%======================================================================
start() -> 
    start1(),
    start2(),
    start3(),
    start4().
    
start1() -> 
    start_one_client(get_config("client1.conf")).

start2() -> 
    start_one_client(get_config("client2.conf")).

start3() -> 
    start_one_client(get_config("client3.conf")).

start4() -> 
    start_one_client(get_config("client4.conf")).


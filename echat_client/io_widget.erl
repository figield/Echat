-module(io_widget).

-export([get_state/1,
	 start/2, 
         test/0, 
	 set_handler/2, 
	 set_prompt/2,
	 set_state/2,
	 set_title/2, 
         insert_str/2, 
         update_state/3,
	 update_buddies/2,
         update_groups/2]).

start(Pid, Title) ->
    gs:start(),
    spawn_link(fun() -> widget(Pid, Title) end).

get_state(Pid)          -> rpc(Pid, get_state).
set_title(Pid, Str)     -> Pid ! {title, Str}.
set_handler(Pid, Fun)   -> Pid ! {handler, Fun}.
set_prompt(Pid, Str)    -> Pid ! {prompt, Str}.
set_state(Pid, State)   -> Pid ! {state, State}.
insert_str(Pid, Str)    -> Pid ! {insert, Str}.
update_state(Pid, N, X) -> Pid ! {updateState, N, X}. 
update_buddies(Pid, Buddies) -> Pid ! {buddies, Buddies}.
update_groups(Pid, Groups) -> Pid ! {groups, Groups}.

rpc(Pid, Q) ->
    Pid ! {self(), Q},
    receive
	{Pid, R} ->
	    R
    end.

widget(Pid, Title) ->
    Size = [{width,600},
            {height,300}],
    Win = gs:window(gs:start(),
		    [{map,true},
                     {title,Title},
                     {configure,true}
                     | Size]),
    gs:frame(packer, Win,[{packer_x, [{stretch,1,100},
                                      {stretch,1,100},
                                      {stretch,1,100},
                                      {stretch,1,100},
                                      {stretch,1,100}]},
			  {packer_y, [{stretch,10,120,100},
                                      {stretch,1,10,10},
                                      {stretch,1,10,10}]}]),
    gs:create(editor,editor,packer, 
              [{pack_x,{1,4}},{pack_y,1},{vscroll,right}]),
    gs:create(entry, entry, packer, 
              [{pack_x,{1, 5}},{pack_y,2},{keypress,true}]),
    gs:create(listbox, buddies, packer, 
              [{pack_x,5},{pack_y,1},{hscroll,false},{vscroll,right}, 
               {click,true},{doubleclick,true}]),
    gs:create(entry,pvt_send_entry,packer, 
              [{pack_x,1},{pack_y,3}]),
    gs:create(button, buddies_button, packer, 
              [{pack_x,2},{pack_y,3}, {label,{text,"Get Buddies"}}]),
    gs:create(entry, nicks_in_group_entry,packer, 
              [{pack_x,3},{pack_y,3}]),
    gs:create(button, nicks_in_group_button, packer, 
              [{pack_x,4},{pack_y,3}, {label,{text,"Buddies in Ch."}}]),
    gs:create(button, groups_button, packer, 
              [{pack_x,5},{pack_y,3}, {label,{text,"Channels"}}]),
    gs:config(packer, Size),
    Prompt = " >",
    State = {"",private},
    gs:config(entry, {insert,{0, Prompt}}),
    gs:config(nicks_in_group_entry, {insert, {0, "(channel)"}}),
    gs:config(pvt_send_entry, {insert, {0, "(buddy)"}}),
    loop(Win, Pid, Prompt, State, fun parse/1). 

loop(Win, Pid, Prompt, State, Parse) ->   
    receive
	{From, get_state} ->
	    From ! {self(), State},
	    loop(Win, Pid, Prompt, State, Parse);
	{handler, Fun} ->
	    loop(Win, Pid, Prompt, State, Fun);
	{prompt, Str} ->
	    gs:config(entry, {delete,{0,last}}),
	    gs:config(entry, {insert,{0,Str}}),
	    loop(Win, Pid, Str, State, Parse);
	{state, S} ->
	    loop(Win, Pid, Prompt, {S, private}, Parse);
	{title, Str} ->
	    io:format("Title:~p~n",[Str]),
	    gs:config(Win, [{title, Str}]),
	    loop(Win, Pid, Prompt, State, Parse);
	{insert, Str} ->
	    gs:config(editor, {insert,{'end',Str}}),
	    scroll_to_show_last_line(),
	    loop(Win, Pid, Prompt, State, Parse);
	{updateState, N, X} ->
            io:format("Update state:~p~n",[{N, X}]),
	    loop(Win, Pid, Prompt, {N, X}, Parse);
	{gs,_,destroy,_,_} ->
	    io:format("Destroyed~n",[]),
	    exit(windowDestroyed);
	{gs, entry,keypress,_,['Return'|_]} ->
	    Text = gs:read(entry, text),
	    %% io:format("Read:~p~n",[Text]),
	    gs:config(entry, {delete,{0,last}}),
	    gs:config(entry, {insert,{0,Prompt}}),
            {A,_} = State,
	    try Parse(Text) of
		Term ->
		    Pid ! {self(), A, Term}
	    catch
		_:_ ->
		    self() ! {insert, "** bad input**\n** /h for help\n"}
	    end,
	    loop(Win, Pid, Prompt, State, Parse);
	{gs,_,configure,[],[W,H,_,_]} ->
	    gs:config(packer, [{width,W},{height,H}]),
	    loop(Win, Pid, Prompt, State, Parse);
	{gs, entry,keypress,_,_} ->
	    loop(Win, Pid, Prompt, State, Parse);
	{buddies, Buddies} ->
	    gs:config(buddies, clear),
	    populate_buddies(Buddies),
	    loop(Win, Pid, Prompt, State, Parse);
	{groups, Groups} ->
	    gs:config(buddies, clear),
	    populate_groups(Groups),
	    loop(Win, Pid, Prompt, State, Parse);
	{gs, nicks_in_group_button, click, _, _} ->
	    Text = gs:read(nicks_in_group_entry, text),
	    Pid ! {self(), {get_nicks_in_group, Text}},
            {A,_} = State,
	    loop(Win, Pid, Prompt, {A, private}, Parse);
	{gs, groups_button, click, _, _} ->
	    Pid ! {self(), get_all_groups},
            {A,_} = State,
	    loop(Win, Pid, Prompt, {A,broadcast}, Parse);
	{gs, buddies_button, click, _, _} ->
	    Pid ! {self(), get_all_buddies},
            {A,_} = State,
	    loop(Win, Pid, Prompt, {A,private}, Parse);
	{gs, buddies, doubleclick, _, [_, Name, _]} ->
            case State of
                {_, broadcast} ->
                    Pid ! {self(), selected_group, Name};
                {_, private} ->
                    gs:config(pvt_send_entry, {delete,{0,last}}),
                    gs:config(pvt_send_entry, {insert,{0,Name}}),
                    Pid ! {self(), selected_buddy, Name}
            end,
            loop(Win, Pid, Prompt, State, Parse);
	{gs, buddies, click, _, [_, Name, _]} ->
            case State of
                {_, broadcast} ->
                    gs:config(nicks_in_group_entry, {delete,{0,last}}),
                    gs:config(nicks_in_group_entry, {insert,{0,Name}});
                {_, private} ->
                    gs:config(pvt_send_entry, {delete,{0,last}}),
                    gs:config(pvt_send_entry, {insert,{0,Name}}),
                    Pid ! {self(), selected_buddy, Name}
            end,
            loop(Win, Pid, Prompt, State, Parse);
	{gs, pvt_send_button, click, _, _} ->
	    To = gs:read(pvt_send_entry, text),
	    Text = gs:read(entry, text),
	    gs:config(entry, {delete,{0,last}}),
	    gs:config(entry, {insert,{0,Prompt}}),
            {A,_} = State,
	    try Parse(Text) of
		Term ->
		    Pid ! {self(), {send_pvt, A, To, Term}}
	    catch
		_:_ ->
		    self() ! {insert, "** bad input**\n** /h for help\n"}
	    end,
        loop(Win, Pid, Prompt, State, Parse);
	Any ->
	    io:format("Discarded:~p~n",[Any]),
	    loop(Win, Pid, Prompt, State, Parse)
    end.

scroll_to_show_last_line() ->
    Size       = gs:read(editor, size),
    Height     = gs:read(editor, height),
    CharHeight = gs:read(editor, char_height),
    TopRow     = Size - Height/CharHeight,
    if  TopRow > 0 -> gs:config(editor, {vscrollpos, TopRow});
	true       -> gs:config(editor, {vscrollpos, 0})
    end.

populate_buddies([]) -> [];
populate_buddies([{_,Nick}|T]) ->
    gs:config(buddies, {add, Nick}),
    populate_buddies(T).

populate_groups([]) -> [];
populate_groups([GroupName | T]) ->
    gs:config(buddies, {add, GroupName}),
    populate_groups(T).

test() ->
    spawn(fun() -> test1() end).

test1() ->
    W = io_widget:start(self(), "Test window"),
    loop(W).

loop(W) ->
    receive
	{W, {str, Str}} ->
	    Str1 = Str ++ "\n",
	    io_widget:insert_str(W, Str1),
	    loop(W)
    end.

parse(Str) ->
    {str, Str}.

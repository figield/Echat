-module(utils).
-author('Dawid Figiel').
-compile(export_all).

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
    ok = gen_tcp:send(Socket, term_to_binary(Data)).

%% @doc
%% Connect to server through TCP.
%% @end
client_connect(Host, Port)->
    gen_tcp:connect(Host, Port, 
                    [binary, 
                     {packet, 0}]).

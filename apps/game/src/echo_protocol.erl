-module(echo_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    loop(Socket, Transport, #{}).

%% loop
loop(Socket, Transport, Profile) ->
    io:format("Socket ~p, Profile ~p~n",[Socket, Profile]),
    case Transport:recv(Socket, 2, infinity) of
        {ok, <<Len:16>>} ->
	    io:format("Recv len ~p~n", [Len]),
	    {ok, Data } = Transport:recv(Socket, Len, infinity), 
	    io:format("Recv dat ~p~n", [binary_to_list(Data)]),
	    {M, D} = unpack(Data),
	    {Method, DataMap} = unpack_raw(M, D),
	    io:format("Method ~p, Data ~p~n",[Method, DataMap]),
	    case control(Method, DataMap) of
		ok -> 
		    io:format("auth success~n",[]),
		    Profile1=maps:put(auth, true, Profile);
		error ->
		    io:format("auth fail~n",[]),
		    Profile1=maps:put(auth, false, Profile)
	    end,
	    Transport:send(Socket, Data),
	    loop(Socket, Transport, Profile1);
	_ ->
	    ok = Transport:close(Socket)
    end.

%% auth
control(auth, DataMap) ->
    User = binary_to_list(maps:get( <<"user">>, DataMap)),
    Password = binary_to_list(maps:get( <<"password">>, DataMap )),
    ok.

unpack_raw(Method, Data) ->
    M = list_to_atom(binary_to_list(Method)),
    Json = jiffy:decode(Data, [return_maps]),
    {M, Json}.    

%% pack & unpack
pack(Method, Data) ->
  MethodLen = length(binary_to_list(Method)),
  <<MethodLen:8, Method/binary, Data/binary>>.

unpack(RawData) ->
  <<MethodLen:8, Bin/binary>> = RawData,
  <<Method:MethodLen/binary, Data/binary>> = Bin,
  {Method, Data}.

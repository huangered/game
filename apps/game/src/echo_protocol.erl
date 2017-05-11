-module(echo_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    io:format("Socket ~p~n",[Socket]),
    case Transport:recv(Socket, 2, infinity) of
        {ok, <<Len:16>>} ->
	    io:format("Recv len ~p~n", [Len]),
	    {ok, Data } = Transport:recv(Socket, Len, infinity), 
	    io:format("Recv dat ~p~n", [binary_to_list(Data)]),
	    {M, D} = unpack(Data),
	    io:format("M ~p, D ~p ~n", [M, D]),           
	    Transport:send(Socket, Data),
	    loop(Socket, Transport);
	_ ->
	    ok = Transport:close(Socket)
    end.

%% pack & unpack
pack(Method, Data) ->
  MethodLen = length(binary_to_list(Method)),
  <<MethodLen:8, Method/binary, Data/binary>>.

unpack(RawData) ->
  <<MethodLen:8, Bin/binary>> = RawData,
  <<Method:MethodLen/binary, Data/binary>> = Bin,
  {Method, Data}.
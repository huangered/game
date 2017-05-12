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
    case receive_line(Socket, Transport) of
	{ok, Method, DataMap} ->
	    case auth(Method, DataMap) of
		{ok, UserId} -> 
		    io:format("auth success~n",[]),
		    Profile1 = Profile#{auth=>true, userId=>UserId},
		    io:format("Enter game loop~n", []),
		    game_loop(Socket, Transport, Profile1);
		{error, Reason} ->
		    io:format("auth fail~n",[]),
		    Profile1=maps:put(auth, false, Profile),
		    loop(Socket, Transport, Profile1)
	    end;
	{error} ->
	    Transport:send(Socket, "error")
    end.
%%
%% game_loop
%% 
game_loop(Socket, Transport, Profile) ->
    case receive_line(Socket, Transport) of
	{ok, Method, DataMap} ->
	    io:format("send echo~n",[]),
	    Transport:send(Socket, "echo~n");
	{error} ->
	    Transport:close(Socket)
	    %%io:format("error",[])
    end,
    game_loop(Socket, Transport, Profile).

%% auth
auth(Method, DataMap) ->
    case Method of
	auth ->
	    User = binary_to_list(maps:get( <<"user">>, DataMap)),
	    Password = binary_to_list(maps:get( <<"password">>, DataMap )),
	    game_account:login(User, Password);
	_ ->
	    io:format("Method ~p not support now~n", [Method]),
	    {error, "Method not support"}
    end.

%%
%% receive line
%%
receive_line(Socket, Transport) ->
    case Transport:recv(Socket, 2, infinity) of
        {ok, <<Len:16>>} ->
	    io:format("Recv len ~p~n", [Len]),
	    case Transport:recv(Socket, Len, infinity) of
		{ok, Data}->
		    io:format("Recv dat ~p~n", [binary_to_list(Data)]),
		    {M, D} = unpack(Data),
		    {Method, DataMap} = unpack_raw(M, D),
		    io:format("Method ~p, Data ~p~n",[Method, DataMap]),
		    {ok, Method, DataMap};
		_ ->
		    Transport:close(Socket),
		    {error}
	    end;
	_ ->
	    Transport:close(Socket),
	    {error}
    end.


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

-module(game_protocol).
-behaviour(ranch_protocol).
-include("record.hrl").

-export([start_link/4, loop/3]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    loop(Socket, Transport, null).

%%
%% socket loop
%% 
loop(Socket, Transport, UserId)	->
    io:format("Protocol socket: ~p with UserId: ~p~n", [Socket, UserId]),
    case receive_line(Socket, Transport) of
	{ok, Method, DataMap} ->
	    game_handle:dispatch(Socket, Transport, Method, DataMap, UserId);
	{error, Reason} ->
	    error_logger:warning_msg("Receive line error, reason~p~n", [Reason]),
	    Transport:close(Socket),
	    game_account:logout(UserId)
    end.
%%
%% receive line
%%
receive_line(Socket, Transport) ->
    case Transport:recv(Socket, 2, infinity) of
        {ok, <<Len:16>>} ->
	    Timeout = 5000,
	    io:format("Recv len ~p, set timeout ~p millionseconds~n", [Len, Timeout]),
	    case Transport:recv(Socket, Len, Timeout) of
		{ok, Data}->
		    {M, D} = game_package:unpack(Data),
		    {Method, DataMap} = game_package:unpack_raw(M, D),
		    {ok, Method, DataMap};
		_ ->
		    error_logger:error_msg("Close socket"),
		    game_storeage:remove_user_detail(Socket),
		    Transport:close(Socket),
		    {error, noreason}
	    end;
	_ ->
	    game_storage:remove_user_detail(Socket),
	    Transport:close(Socket),
	    {error, noreason}
    end.




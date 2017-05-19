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
	    case auth(Method, DataMap, Socket, Transport) of
		{ok, UserId} -> 
		    error_logger:info_msg("Auth success, User id ~p~n",[UserId]),
            Player_pid = game_player_sup:new_player([]),
		    Profile1 = Profile#{auth=>true, userId=>UserId, playerPid=>Player_pid},
		    error_logger:info_msg("Enter game loop, User id: ~p, Player pid: ~p~n", [UserId, Player_pid]),
		    game_loop(Socket, Transport, Profile1);
		{error, Reason} ->
		    error_logger:info_msg("auth fail~n",[]),
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
    UserId = maps:get(userId, Profile),
    case receive_line(Socket, Transport) of
    {ok, heartbeat, _} ->
        io:format("Hearbeat", []),
        game_loop(Socket, Transport, Profile);
    {ok, logout, _} ->
        game_account:logout(UserId),
        Transport:close(Socket);    
	{ok, Method, DataMap} ->
        Pid = maps:get(playerPid, Profile),
	    io:format("Send request to player pid ~p~n",[Pid]),
        game_player:action(Pid, UserId, Method, DataMap),
        game_loop(Socket, Transport, Profile);
	{error} ->
        error_logger:warning_msg("logout~n", []),
	    Transport:close(Socket),
        game_account:logout(UserId)
    end.

%% auth
auth(Method, DataMap, Socket, Transport) ->
    case Method of
	auth ->
	    User = maps:get( "user", DataMap),
	    Password = maps:get( "password", DataMap ),
	    game_account:login(User, Password, Socket, Transport);
	_ ->
	    error_logger:warning_msg("Method ~p not support now~n", [Method]),
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
		    {M, D} = game_package:unpack(Data),
		    {Method, DataMap} = game_package:unpack_raw(M, D),
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




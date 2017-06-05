-module(game_handle).

-export([dispatch/5,
		 talkResp/3]).

dispatch(Socket, Transport, Method, DataMap, UserId) ->
    case Method of
	auth ->
	    {ok, ID} = auth(DataMap),
	    game_storage:register_socket(ID, Socket, Transport),
	    authResp(Socket, Transport, <<"ok">>),
	    game_protocol:loop(Socket, Transport, ID);
	heartbeat ->
	    io:format("Hearbeat~n", []),
	    game_protocol:loop(Socket, Transport, UserId);
	logout ->
	    game_account:logout(UserId);
	talk->
	    TargetId = maps:get("userId", DataMap),
	    Msg = maps:get("msg", DataMap),
	    game_account:send_msg(UserId, TargetId, Msg),
	    game_protocol:loop(Socket, Transport, UserId);
	show ->
	    game_account:show(UserId),
	    game_protocol:loop(Socket, Transport, UserId);
	list_player ->
	    game_account:list_player(UserId),
	    game_protocol:loop(Socket, Transport, UserId);
	enter_in_game ->
	    PlayerId = maps:get("playerId", DataMap),
	    game_account:enter_in_game(UserId, PlayerId),
	    game_protocol:loop(Socket, Transport, UserId);
	Method ->
	    Pid = game_storage:lookup_user_detail_player_process_id(UserId),
	    io:format("Send request to player pid ~p~n",[Pid]),
	    game_player:action(Pid, UserId, Method, DataMap),
	    game_protocol:loop(Socket, Transport, UserId);
	{error} ->
	    error_logger:warning_msg("logout~n", []),
	    Transport:close(Socket),
	    game_account:logout(UserId)
    end.


%% auth
auth(DataMap) ->
    User = maps:get("user", DataMap),
	Password = maps:get( "password", DataMap ),
	case game_account:login(User, Password) of
		{ok, Id} ->
			game_storage:insert_user_detail(Id, 0, 0, 0, 0),
			{ok, Id};
		{error, Msg} ->
			io:format("Login fail, reason: ~p~n", [Msg]),
			{error, "login fail"}
	end.
%% auth response
authResp(S, T, Msg) ->	
    Bin = game_package:pack(<<"AuthResp">>, Msg),
    MsgPack = game_package:msg_pack(Bin),
    T:send(S, MsgPack).

%% talk response
talkResp(S, T, Msg) ->	
    Bin = game_package:pack(<<"TalkResp">>, Msg),
    MsgPack = game_package:msg_pack(Bin),
    T:send(S, MsgPack).

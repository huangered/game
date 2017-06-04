%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author huang yi <huangered@hotmail.com>
%% @copyright 2017

-module(game_storage).

-behaviour(gen_server).

-include("record.hrl").

-export([start_link/0]).


-export([insert_user_detail/5,
		 lookup_user_detail/1,
		 remove_user_detail/1,
		 lookup_user_detail_player_process_id/1,
		 register_socket/3,
		 query_socket/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

insert_user_detail(UserId, PlayerId, WorldId, PlayerProc, WorldProc) ->
	gen_server:call(?MODULE, {insert, UserId, PlayerId, WorldId, PlayerProc, WorldProc}).

lookup_user_detail(UserId) ->
	gen_server:call(?MODULE, {lookup, UserId}).

remove_user_detail(UserId) ->
	gen_server:call(?MODULE, {remove, UserId}).	

lookup_user_detail_player_process_id(UserId) ->
	gen_server:call(?MODULE, {player_proc_id, UserId}).

register_socket(UserId, Socket, Transfer) ->
	gen_server:call(?MODULE, {register_socket, UserId, Socket, Transfer}).

query_socket(UserId) ->
	gen_server:call(?MODULE, {query_socket, UserId}).
%% private method

init([]) ->
    error_logger:info_msg("ETS Storage init.", []),
	UserTable = ets:new(user_detail_db, [named_table, {keypos, #user_detail.user_id}]),
	SockTable = ets:new(user_socket_db, [named_table, {keypos, #user_socket.user_id}]),	
    {ok, #state{}}.

handle_call({insert, UserId, PlayerId, WorldId, PlayerProc, WorldProc}, _From, State) ->
	io:format("Insert user socket ~p~n", [UserId]),
	ets:insert(user_detail_db, #user_detail{user_id=UserId, player_id=PlayerId, world_id=WorldId,
	 										player_proc=PlayerProc, world_proc=WorldProc}),
    {reply, ok, State};

handle_call({lookup, UserId}, _From, State) ->
	Data = ets:lookup(user_detail_db, UserId),
	case Data of
		[V] -> {reply, V, State};
		[] -> {reply, {ignore}, State}
	end;

handle_call({player_proc_id, UserId}, _From, State) ->
	Data = ets:lookup(user_detail_db, UserId),
	case Data of
		[V] ->
			{user_detail, _, _, _, Player_pid, _} = V,
			{reply, Player_pid, State};
		[] -> {reply, ignore, State}
	end;

handle_call({remove, UserId}, _From, State) ->
	io:format("Remove user socket ~p~n", [UserId]),
	ets:delete(user_detail_db, UserId),
	{reply, ok, State};

handle_call({register_socket, UserId, Socket, Transfer}, _From, State) ->
	ets:insert(user_socket_db, #user_socket{user_id=UserId, socket=Socket, transfer=Transfer}),
	{reply, ok, State};

handle_call({query_socket, UserId}, _From, State) ->
	[Data] = ets:lookup(user_socket_db, UserId),
	io:format("Lookup data:~p~n", [Data]),
	{_, _, Socket, Transfer} = Data,
	{reply, {ok, Socket, Transfer}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

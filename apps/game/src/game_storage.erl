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
		 remove_user_detail/1]).

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

%% private method

init([]) ->
	UserTable = ets:new(user_detail_db, [named_table, {keypos, #user_detail.user_id}]),	
    {ok, #state{}}.

handle_call({insert, UserId, PlayerId, WorldId, PlayerProc, WorldProc}, _From, State) ->
	ets:insert(user_detail_db, #user_detail{user_id=UserId, player_id=PlayerId, world_id=WorldId,
	 										player_proc=PlayerProc, world_proc=WorldProc}),
    {reply, ok, State};

handle_call({lookup, UserId}, _From, State) ->
	Data = ets:lookup(user_detail_db, UserId),
	{reply, {ok, Data}, State};

handle_call({remove, UserId}, _From, State) ->
	ets:delete(user_detail_db, UserId),
	{reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

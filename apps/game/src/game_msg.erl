%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author huang yi <huangered@hotmail.com>
%% @copyright 2017

-module(game_msg).

-behaviour(gen_server).

-export([start_link/0]).

-export([send_package/3,
         get_msg/1,
         save_msg/3]).

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

send_package(UserId, Method, Data) ->
    gen_server:call(?MODULE, {send_package, UserId, Method, Data}).

get_msg(UserId) ->
    gen_server:call(?MODULE, {get_msg, UserId}).

save_msg(UserId, TargetId, Msg) ->
    gen_server:cast(?MODULE, {save_msg, UserId, TargetId, Msg}).

%% private

init([]) ->
    {ok, #state{}}.

handle_call({get_msg, UserId}, _From, State) ->
    {reply, ignored, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({save_msg, UserId, TargetId, Msg}, State) ->
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

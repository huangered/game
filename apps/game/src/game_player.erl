%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author huang yi <huangered@hotmail.com>
%% @copyright 2017

-module(game_player).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export ([action/4]).

-record(state, {players}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

action(M, User, Action, Data) ->
    gen_server:call(M, {User, Action, Data}).

init([]) ->
    io:format("player init~n", []),
    {ok, #state{ players = dict:new() }}.

handle_call({User, Action, Data}, _From, State) ->
    case Action of
        move -> io:format("move~n", []);
        attack -> io:format("attack~n", []);
        world -> io:format("world~n",[]);
        _ -> io:format("no support", [])
    end,
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

move(Player) ->
    error_logger:info_msg("Move").

attack(Player) ->
    error_logger:info_msg("Attack").

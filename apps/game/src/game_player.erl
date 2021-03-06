%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author huang yi <huangered@hotmail.com>
%% @copyright 2017

-module(game_player).

-behaviour(gen_server).

-include("record.hrl").

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export ([action/4]).

-record(state, {player}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

action(M, User, Action, Data) ->
    gen_server:call(M, {User, Action, Data}).

init([]) ->
    io:format("player init~n", []),
    P = #player{x=0,y=0,hp=0,mp=0},
    {ok, #state{ player = P }}.

handle_call({User, Action, Data}, _From, State=#state{player=P}) ->
    case Action of
        move -> 
            X = maps:get("x", Data) + P#player.x,
            Y = maps:get("y", Data) + P#player.y,
            P2 = P#player{x=X, y=Y};
        attack -> 
            P2 = P,
            io:format("attack~n", []);
        world -> 
            P2 = P,
            io:format("world~n",[]);
        hp ->
            Hp = maps:get("hp", Data) + P#player.hp,
            P2 = P#player{hp = Hp},
            io:format("hp~n", []);
        mp ->
            Mp = maps:get("mp", Data) + P#player.mp,
            P2 = P#player{mp = Mp},
            io:format("mp~n", []);
        _ ->
            P2 = P,
            io:format("no support~p~n", [Action])
    end,
    error_logger:info_msg("Update player: ~p~n", [P2]),
    {reply, ignored, #state{player = P2}}.

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

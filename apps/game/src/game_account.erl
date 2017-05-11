%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @copyright 2011 Opscode, Inc.

-module(game_account).

-behaviour(gen_server).

-export([start_link/0,
         login/2,
         logout/1,
         add/1]).

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

add(User) ->
    gen_server:call(?MODULE, {add, User}).

login(User, Password) ->
    gen_server:call(?MODULE, {login, User, Password}).

logout(User) ->
    gen_server:call(?MODULE, {logout, User}).

init([]) ->
    {ok, #state{}}.

handle_call({add, User}, _From, State) ->
    io:format("Add user: ~p~n", [User]),         
    {reply, ignored, State};

handle_call({login, User, Password}, _From, State) ->
    io:format("Login~n",[]),
    {reply, ignored, State};

handle_call({logout, User}, _From, State) ->
    io:format("Logout~n", []),
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

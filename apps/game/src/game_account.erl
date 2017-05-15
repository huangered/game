%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author huang yi <huangered@hotmail.com>
%% @copyright 2017

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

-record(state, {users}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(User) ->
    gen_server:call(?MODULE, {add, User}).

login(User, Password) ->
    gen_server:call(?MODULE, {login, User, Password}).

logout(User) ->
    gen_server:call(?MODULE, {logout, User}).

init([]) ->
    Users = dict:store("abcd","abcd",dict:new()),
    {ok, #state{users=Users}}.

handle_call({add, User}, _From, State=#state{users=Users}) ->
    io:format("Add user: ~p~n", [User]),
    UsersN=dict:store(User, User, Users),
    {reply, ignored, #state{users=UsersN}};
%%
%% @return {ok, UserId} | {error, Reason}
%%
handle_call({login, User, Password}, _From, State=#state{users=Users}) ->
    Conn = open(),
    {ok, _, Res} = epgsql:equery(Conn, "select * from users where username=$1 and password=$2", [ User, Password]),
    close(Conn),
    io:format("DB:~p~n", [Res]),
    case Res of 
        [{ID, _, _}] -> 
            {reply, {ok, ID}, State};
        [] -> 
            {reply, {error, "not found"}, State}
    end;

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
open() ->
    {ok, Conn}=epgsql:connect("localhost", "huangered", "",
                              [
                               { database,"huangered",
                                 timeout, 5000
                               }]),
    Conn.

close(Conn) ->
   ok = epgsql:close(Conn).

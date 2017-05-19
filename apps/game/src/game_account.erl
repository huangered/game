%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author huang yi <huangered@hotmail.com>
%% @copyright 2017

-module(game_account).
-behaviour(gen_server).

-export([start_link/0,
         login/4,
         logout/1,
         add/1,
         send_msg/3,
         show/0]).

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

login(User, Password, Socket, Transfer) ->
    gen_server:call(?MODULE, {login, User, Password, Socket, Transfer}).

logout(UserId) ->
    gen_server:call(?MODULE, {logout, UserId}).

send_msg(SenderId, UserId, Msg) ->
    gen_server:call(?MODULE, {send_msg, SenderId, UserId, Msg}).

show() ->
    gen_server:call(?MODULE, {show}).

init([]) ->
    {ok, #state{users=dict:new()}}.

handle_call({add, User}, _From, State=#state{users=Users}) ->
    io:format("Add user: ~p~n", [User]),
    UsersN=dict:store(User, User, Users),
    {reply, ignored, #state{users=UsersN}};
%%
%% @return {ok, UserId} | {error, Reason}
%%
handle_call({login, User, Password, Socket, Transfer}, _From, State=#state{users=Users}) ->
    Conn = open(),
    {ok, _, Res} = epgsql:equery(Conn, "select * from users where username=$1 and password=$2", [ User, Password]),
    close(Conn),
    error_logger:info_msg("DB access: ~p~n", [Res]),
    case Res of 
        [{ID, _, _}] -> 
            U = dict:store(ID, {Socket, Transfer}, Users),
            {reply, {ok, ID}, #state{users = U}};
        [] -> 
            {reply, {error, "not found"}, State}
    end;

handle_call({logout, UserId}, _From, State=#state{users=Users}) ->
    io:format("Logout user id: ~p~n", [UserId]),
    U = dict:erase(UserId, Users),
    {reply, ok, #state{users=U}};

handle_call({send_msg, SenderId, UserId, Msg}, _From, State=#state{users=Users}) ->
    {Socket, Transfer} = dict:get(UserId, Users),
    Transfer:send(Socket, Msg),
    {reply, ignored, State};

handle_call({show}, _From, State=#state{users=Users}) ->
    error_logger:info_msg("Show accounts: ~p~n", [Users]),
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
open() ->
    {ok, Conn}=epgsql:connect("localhost", "huangered", "",
                              [
                               { database,"huangered",
                                 timeout, 5000
                               }]),
    Conn.

close(Conn) ->
   ok = epgsql:close(Conn).

%% new player pid
new_player() ->
    {ok, Pid } = superviosr:start_child(game_player_sup, []).

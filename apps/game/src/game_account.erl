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
         show/1,
         create_player/0]).

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

%%
%% @return {ok, users} 
%%
show(UserId) ->
    gen_server:call(?MODULE, {show, UserId}).

create_player() ->
    gen_server:call(?MODULE, {create_player}).

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
    Conn = game_db:open(),
    Res = game_db:select_user(Conn, User, Password),
    game_db:close(Conn),
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
    {ok, {Socket, Transfer}} = dict:find(UserId, Users),
    error_logger:info_msg("Send msg: socket ~p, Transfer ~p~n", [Socket, Transfer]),
    Transfer:send(Socket, Msg),
    {reply, ignored, State};

handle_call({show, UserId}, _From, State=#state{users=Users}) ->
    {ok, {Socket, Transfer}} = dict:find(UserId, Users),
    Conn = game_db:open(),
    Users1 = game_db:select_users(Conn),
    game_db:close(Conn),
    error_logger:info_msg("Show accounts in db: ~p~n", [Users1]),
    Data = lists:foldl(fun game_package:parse_user/2, [], Users1),
    Json = jiffy:encode(Data),
    Bin = game_package:pack(<<"show">>, Json),
    MsgPack = game_package:msg_pack(Bin),
    Transfer:send(Socket, MsgPack),
    {reply, ok, State};

handle_call({create_player}, _From, State) ->
    error_logger:info_msg("Create player: ~n", []),
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

%% new player pid
new_player() ->
    {ok, Pid } = superviosr:start_child(game_player_sup, []).

%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author huang yi <huangered@hotmail.com>
%% @copyright 2017

-module(game_account).
-behaviour(gen_server).

-export([start_link/0,
         login/2,
         logout/1,
         add/1,
         send_msg/3,
         show/1,
         list_player/1,
         select_player/0,
         add_player/0,
         del_player/0,
         enter_in_game/2,
         enter_out_game/0]).

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

logout(UserId) ->
    gen_server:call(?MODULE, {logout, UserId}).

send_msg(SenderId, UserId, Msg) ->
    gen_server:call(?MODULE, {send_msg, SenderId, UserId, Msg}).

list_player(UserId) ->
    gen_server:call(?MODULE, {list_player, UserId}).
select_player() ->    
    ok.
add_player() ->
    ok.
del_player() ->
    ok.
enter_in_game(UserId, PlayerId) ->
    gen_server:call(?MODULE, {enter_in_game, UserId, PlayerId}).
enter_out_game() ->
    ok.

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
handle_call({login, User, Password}, _From, State=#state{users=Users}) ->
    Conn = game_db:open(),
    Res = game_db:select_user(Conn, User, Password),
    game_db:close(Conn),
    error_logger:info_msg("DB access: ~p~n", [Res]),
    case Res of 
        [{ID, _, _}] -> 
            U = dict:store(ID, calendar:local_time(), Users),
            {reply, {ok, ID}, #state{users = U}};
        [] -> 
            {reply, {error, "not found"}, State}
    end;

handle_call({logout, UserId}, _From, State=#state{users=Users}) ->
    io:format("Logout user id: ~p~n", [UserId]),
    U = dict:erase(UserId, Users),
    {reply, ok, #state{users=U}};

handle_call({send_msg, SenderId, UserId, Msg}, _From, State=#state{users=Users}) ->
    {ok, Socket, Transfer} = game_storage:query_socket(UserId),
    error_logger:info_msg("Send msg: socket ~p, Transfer ~p~n", [Socket, Transfer]),
    Transfer:send(Socket, Msg),
    {reply, ignored, State};

handle_call({show, UserId}, _From, State=#state{users=Users}) ->
    {ok, Socket, Transfer} = game_storage:query_socket(UserId),
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

handle_call({list_player, UserId}, _From, State=#state{users=Users}) ->
    {ok, Socket, Transfer} = game_storage:query_socket(UserId),
    Conn = game_db:open(),
    Players = game_db:list_player(Conn, UserId),
    game_db:close(Conn),
    io:format("list user: ~p~n", [Players]),
    Data = lists:foldl(fun game_package:parse_player/2, [], Players),
    Bin = game_package:pack(<<"list_player_resp">>, jiffy:encode(Data)),
    MsgPack = game_package:msg_pack(Bin),
    Transfer:send(Socket, MsgPack),
    {reply, ok, State};

handle_call({create_player}, _From, State) ->
    error_logger:info_msg("Create player: ~n", []),
    {reply, ok, State};

handle_call({enter_in_game, UserId, PlayerId}, _From, State) ->
    error_logger:info_msg("Player ~p enter in game.~n", [PlayerId]),
    Pid = game_player_sup:new_player([]),
    {user_detail, U_id, P_id, W_id, P_pid, W_pid} = game_storage:lookup_user_detail(UserId),
    game_storage:insert_user_detail(U_id, PlayerId, W_id, Pid, W_pid),
    io:format("Player pid:~p~n", [Pid]),
    {reply, ok, State};
handle_call({enter_out_game, PlayerId}, _From, State) ->
    error_logger:info_msg("Player ~p enter out game.~n", [PlayerId]),
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

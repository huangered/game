%%%-------------------------------------------------------------------
%% @doc game top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(game_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Account = {game_account, {game_account, start_link, []},
               permanent, brutal_kill, worker, [game_account]},
    Game_storage = {game_storage, {game_storage, start_link, []},
                    permanent, brutal_kill, worker, [game_storage]},
    Player_sup = {game_player_sup, {game_player_sup, start_link, []},
                  permanent, brutal_kill, supervisor, [game_player_sup]},
    World_sup = {game_world_sup, {game_world_sup, start_link, []},
                 permanent, brutal_kill, supervisor, [game_world_sup]},

    {ok, { {one_for_all, 0, 1}, [Account, Game_storage, Player_sup, World_sup]} }.


%%====================================================================
%% Internal functions
%%====================================================================

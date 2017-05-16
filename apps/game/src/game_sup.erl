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
    Player_sup = {game_player_sup, {game_player_sup, start_link, []},
		  permanent, brutal_kill, supervisor, [game_player_sup]},
    {ok, { {one_for_all, 0, 1}, [Account, Player_sup]} }.


%%====================================================================
%% Internal functions
%%====================================================================

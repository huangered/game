%%%-------------------------------------------------------------------
%% @doc game top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(game_player_sup).

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
    Player = {game_player, {game_player, start_link, []},
               permanent, brutal_kill, worker, [game_player]},    
    {ok, { {simple_one_for_one, 0, 1}, [Player]} }.


%%====================================================================
%% Internal functions
%%====================================================================

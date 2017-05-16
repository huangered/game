%%%-------------------------------------------------------------------
%% @doc game top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(game_player_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([new_player/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

new_player([]) ->
	{ok, Pid} = supervisor:start_child(game_player_sup, []),
	Pid.

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

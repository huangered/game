%%%-------------------------------------------------------------------
%% @doc game top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(game_world_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([new_world/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

new_world([]) ->
	{ok, Pid} = supervisor:start_child(game_world_sup, []),
	Pid.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    World = {game_world, {game_world, start_link, []},
               permanent, brutal_kill, worker, [game_world]},    
    {ok, { {simple_one_for_one, 0, 1}, [World]} }.


%%====================================================================
%% Internal functions
%%====================================================================

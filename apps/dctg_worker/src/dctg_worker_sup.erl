-module(dctg_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 2000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ClientSup = {dctg_client_sup, {dctg_client_sup, start_link, []}, permanent, 2000, supervisor, [dctg_client_sup]},
    Launcher = ?CHILD(dctg_launcher, worker),
    StatCache = ?CHILD(dctg_stat_cache, worker),
    Killer = ?CHILD(dctg_client_killer, worker),
    {ok, { {one_for_one, 5, 10}, [ClientSup, Launcher, StatCache, Killer]} }.


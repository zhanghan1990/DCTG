-module(dctg_sup).

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
    Controller = ?CHILD(dctg_controller, worker),
    Launch = ?CHILD(dctg_start_launcher, worker),
    Config = ?CHILD(dctg_config_server, worker),
    Mon = ?CHILD(dctg_monitor, worker),
    {ok, { {one_for_one, 5, 10}, [Controller, Launch, Config, Mon]} }.


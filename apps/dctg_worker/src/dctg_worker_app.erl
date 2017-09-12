-module(dctg_worker_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    error_logger:logfile({open, atom_to_list(node()) ++ ".log"}),
    dctg_worker_sup:start_link().

stop(_State) ->
    ok.

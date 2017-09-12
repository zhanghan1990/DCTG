-module(dctg_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %error_logger:tty(false),
    error_logger:logfile({open, "controller.log"}),
    error_logger:info_msg("WJY: dctg start~n", []),
    dctg_sup:start_link().

stop(_State) ->
    ok.

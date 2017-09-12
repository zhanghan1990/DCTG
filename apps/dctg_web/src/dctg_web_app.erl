%% @author Mochi Media <dev@mochimedia.com>
%% @copyright dctg_web Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the dctg_web application.

-module(dctg_web_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for dctg_web.
start(_Type, _StartArgs) ->
    dctg_web_deps:ensure(),
    dctg_web_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for dctg_web.
stop(_State) ->
    ok.

%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc dctg_web.

-module(dctg_web).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the dctg_web server.
start() ->
    dctg_web_deps:ensure(),
    ensure_started(crypto),
    application:start(dctg_web).


%% @spec stop() -> ok
%% @doc Stop the dctg_web server.
stop() ->
    application:stop(dctg_web).

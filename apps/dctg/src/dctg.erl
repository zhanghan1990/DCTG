-module(dctg).

-export([start/0, startworker/0, shutdown/0]).

start() ->
    application:start(crypto),
    application:start(emysql),
    application:start(dctg).

startworker() ->
    application:start(dctg_worker),
    {ok, Node} = application:get_env(dctg_worker, controller),
    dctg_config_server:init_fin(Node).

shutdown() ->
    application:stop(dctg).

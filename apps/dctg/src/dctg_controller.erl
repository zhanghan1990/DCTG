-module(dctg_controller).

-behaviour(gen_server).

-export([start_link/0, stop/0, start_launchers/0, status/0, finish/0, set_status/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_launchers() ->
    gen_server:call(?MODULE, {start_launchers}, infinity).

stop() ->
    gen_server:cast(?MODULE, {stop}).

status() ->
    gen_server:call(?MODULE, {status}).

finish() ->
    gen_server:cast(?MODULE, {finish}).

set_status(Status) ->
    gen_server:cast(?MODULE, {set_status, Status}).

init([]) ->
    %error_logger:info_msg("WJY: controller init~n"),
    {ok, wait}.

handle_call({start_launchers}, _From, State) when State =:= running orelse State =:= init ->
    {reply, error, State};
handle_call({start_launchers}, _From, _State) ->
    dctg_start_launcher:newbeams(),
    {reply, ok, init};

handle_call({status}, _From, State) ->
    {reply, State, State}.

handle_cast({finish}, _State) ->
    dctg_start_launcher:stop(),
    dctg_monitor:stop(),
    dctg_config_server:stop(),
    {noreply, finish};

handle_cast({stop}, _State) ->
    dctg_start_launcher:stop(),
    dctg_monitor:stop(),
    dctg_config_server:stop(),
    {noreply, stop};

handle_cast({set_status, Status}, _State) ->
    {noreply, Status}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_R, _State) ->
    ok.

code_change(_R, State, _E) ->
    {ok, State}.

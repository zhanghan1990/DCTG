-module(dctg_config_server).

-behaviour(gen_server).

-export([start_link/0, get_config/2, set_hostip/2,
        set_total/1, set_config/2, stop/0,
        set_launcher_per_ip/1, start_send/1,
        init_fin/1, finish/1, get_hostlist/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    launcher = 0,
    start = 0,
    total,
    config,
    finish = 0,
    hostlist,
    iparray,
    count_arr
    }).

-include("config.hrl").

-define(FINISH_DELAY, 2000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_config(Config, CountArr) ->
    gen_server:cast(?MODULE, {set_config, Config, CountArr}).

get_config(ID, Node) ->
    gen_server:call({?MODULE, Node}, {get_config, ID}).

set_hostip(HostList, IPArray) ->
    gen_server:cast(?MODULE, {set_hostip, HostList, IPArray}).

set_total(Num) ->
    gen_server:cast(?MODULE, {set_total, Num}).

%only can be called after set_total and set_hostip and before set_config
set_launcher_per_ip(Num) ->
    gen_server:cast(?MODULE, {set_launcher_per_ip, Num}).

init_fin(Node) ->
    gen_server:cast({?MODULE, Node}, {init_fin}).

start_send(Node) ->
    gen_server:cast({?MODULE, Node}, {start_send}).

finish(Node) ->
    gen_server:cast({?MODULE, Node}, {finish}).

get_hostlist() ->
    gen_server:call(?MODULE, {get_hostlist}).

stop() ->
    gen_server:cast(?MODULE, {stop}).

init([]) ->
    {ok, #state{}}.

handle_call({get_config, ID}, _From, State = #state{config = Config, iparray = Arr, count_arr = CArr}) ->
    IP = array:get(ID, Arr),
    Count = array:get(ID, CArr),
    {reply, {Config, IP, Count}, State};

handle_call({get_hostlist}, _From, State = #state{hostlist = HostList}) ->
    {reply, HostList, State}.

handle_cast({set_total, Num}, State) ->
    {noreply, State#state{total = Num}};

handle_cast({set_config, Config, CountArr}, State) when is_record(Config, config) ->
    {noreply, State#state{config = Config, count_arr = CountArr}};

handle_cast({set_hostip, HostList, IPArray}, State)
        when is_list(HostList) ->
    case array:is_array(IPArray) of
        true ->
            {noreply, State#state{hostlist = HostList, iparray = IPArray}};
        _ ->
            error_logger:info_msg("WJY: config server set host ip Error!"),
            {noreply, State}
    end;

handle_cast({set_launcher_per_ip, Num}, State =
    #state{total = Total, hostlist = HostList, iparray = IPArray}) ->
    NewTotal = Total * Num,
    NewHostList = lists:flatten(lists:map(fun(H) -> lists:duplicate(Num, H) end, HostList)),
    IPList = array:to_list(IPArray),
    NewIPList = lists:flatten(lists:map(fun(I) -> lists:duplicate(Num, I) end, IPList)),
    NewIPArray = array:from_list(NewIPList),
    {noreply, State#state{total = NewTotal, hostlist = NewHostList, iparray = NewIPArray}};

handle_cast({init_fin}, State = #state{launcher = Count, total = Num}) ->
    %error_logger:info_msg("WJY: config server init fin received~n"),
    if
        Count + 1 >= Num ->
            %error_logger:info_msg("WJY: enough~n"),
            dctg_controller:set_status(starting),
            dctg_start_launcher:launch_start();
        true ->
            ok
    end,
    {noreply, State#state{launcher = Count + 1}};

handle_cast({start_send}, State = #state{start = Count, total = Num}) ->
    if
        Count + 1 >= Num ->
            dctg_controller:set_status(running);
        true ->
            ok
    end,
    {noreply, State#state{start = Count + 1}};

handle_cast({finish}, State = #state{total = Total, finish = Fin}) ->
    %error_logger:info_msg("WJY: config server: finish ~p~n", [Fin]),
    NewFin = Fin + 1,
    if
        NewFin >= Total ->
            timer:sleep(?FINISH_DELAY),
            error_logger:info_msg("WJY: config server: all finished!!!~n"),
            dctg_controller:finish(),
            {noreply, State#state{finish = 0}};
        true ->
            {noreply, State#state{finish = NewFin}}
    end;


handle_cast({stop}, _State) ->
    % WJYTODO: should change this module to gen_fsm so that the get_config call will not fail
    error_logger:info_msg("WJY: config server stop~n"),
    {noreply, #state{}}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_Old, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

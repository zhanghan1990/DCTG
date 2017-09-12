-module(dctg_launcher).

-behaviour(gen_fsm).

-include("dctg_record.hrl").

-export([start_link/0, launch/1]).
-export([init/1, launcher/2, wait/2,
    waitraw/2, launchraw/2,
    handle_event/3, handle_sync_event/4,
    handle_info/3, terminate/3, code_change/4]).

-define(WARN_THRESH, 0.2).
-define(ETH_P_ALL, 16#0003).

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

launch({Node, StartTime}) ->
    %error_logger:info_msg("WJY: send launch~n"),
    gen_fsm:send_event({?MODULE, Node}, {launch, StartTime}).

init([]) ->
    %error_logger:info_msg("WJY: launcher init~n"),
    ID = utils:get_id(),
    {ok, ControllerNode} = application:get_env(dctg_worker, controller),
    case catch dctg_config_server:get_config(ID, ControllerNode) of
        {Config, IP, Count} when is_record(Config, config) ->
            Type = Config#config.type,
            Intensity = Config#config.intensity,
            DestList = Config#config.dutlist, % dutlist is actually a tuple
            Content = Config#config.protocol,
            if
                Intensity * 10 < 1 ->
                    Interval = 100,
                    NewIntensity = Intensity * 100;
                Intensity < 1 ->
                    NewIntensity = Intensity * 10,
                    Interval = 10;
                true ->
                    NewIntensity = Intensity * 10, % WJY using 10ms interval
                    Interval = 10
            end,
            case Type of
                http ->
                    Port = Content#http.port,
                    URL = Content#http.content,
                    RequestInterval = Content#http.interval,
                    StartAfter = Content#http.start_time,
                    State = #launcher_http{
                                        ip = IP,
                                        intensity = NewIntensity,
                                        count = Count,
                                        dest = DestList,
                                        interval = Interval,
                                        port = Port,
                                        url = URL,
                                        req_interval = RequestInterval,
                                        start_after = StartAfter,
                                        fraction = 0,
                                        round = 0,
                                        nth = 1
                                    },
                    {ok, wait, State};
                raw ->
                    if
                        Count =:= 0 ->
                            NewCount = -1; % dirty trick to send raw packet infinitely when count is 0
                        true ->
                            NewCount = Count
                    end,
                    Data = Content#raw.data,
                    DataLen = Content#raw.len,
                    SrcDev = "eth0",
                    SrcMac = send_raw_packet:get_src_mac(SrcDev),
                    %% open a PF_PACKET raw socket with ETH_P_ALL
                    Path = procket_mktmp:name("/tmp/procket_sock_XXXXXXXXXXXX"), % WJYWARN:Path may conflict
                    {ok, Socket} = procket:open(0, [{protocol, procket:ntohs(?ETH_P_ALL)},
                                                    {family, packet}, {type, raw},
                                                    {pipe, Path}]),
                    Ifindex = packet:ifindex(Socket, SrcDev),
                    ok = packet:bind(Socket, Ifindex),
                    State = #launcher_raw{
                                intensity = NewIntensity,
                                count = NewCount,
                                dest = DestList,
                                interval = Interval,
                                sock = Socket,
                                src_mac = SrcMac,
                                data = Data,
                                datalen = DataLen,
                                fraction = 0,
                                round = 0,
                                nth = 1
                    },
                    {ok, waitraw, State}
            end;
        Other ->
            error_logger:info_msg("WJY: get_config failed~n"),
            exit({error, Other})
    end.

wait({launch, StartTime}, State) when is_record(State, launcher_http)->
    %error_logger:info_msg("WJY: launch start~n"),
    Time = case utils:timediff(StartTime, os:timestamp()) of
                Num when Num < 0 ->
                    0;
                Else ->
                    erlang:trunc(Else)
            end,
    gen_fsm:send_event_after(Time, {launch}),
    dctg_stat_cache:start_send(StartTime),
    {next_state, launcher, State#launcher_http{start_time = StartTime}}.

launcher({launch}, State=#launcher_http{count = Count}) when Count =< 0 ->
    dctg_client_killer:kill_finish(),
    {stop, normal, State};
launcher({launch}, State=#launcher_http{
                                    ip = IP,
                                    intensity = Intensity,
                                    count = Count,
                                    dest = DestList,
                                    interval = Interval,
                                    start_time = StartTime,
                                    port = Port,
                                    url = URL,
                                    req_interval = RInterval,
                                    start_after = StartAfter,
                                    fraction = Frac,
                                    round = Round,
                                    nth = Nth
                                    }) ->
    if
        Round =:= 0 ->
            {ok, ControllerNode} = application:get_env(dctg_worker, controller),
            dctg_config_server:start_send(ControllerNode);
        true ->
            ok
    end,
    %error_logger:info_msg("WJY: launch, Count: ~p~n", [Count]),
    CurrentTime = os:timestamp(), % TODO: should be erlang:now()?
    TimePast = utils:timediff(StartTime, CurrentTime),
    Timer = case TimePast + Interval * (Round + 1) of
                Num when Num < 0 ->
                    %error_logger:info_msg("WJY: launcher: too high load!~n"),
                    0;
                Else ->
                    Else
            end,
    gen_fsm:send_event_after(erlang:round(Timer), {launch}),
    NewIntensity = erlang:trunc(Intensity),
    NewFrac = Frac + Intensity - NewIntensity,
    if
        NewFrac > 1 ->
            NewFrac2 = NewFrac - 1,
            NewIntensity2 = NewIntensity + 1;
        true ->
            NewFrac2 = NewFrac,
            NewIntensity2 = NewIntensity
    end,
    RNum = erlang:min(NewIntensity2, Count),
    NewNth = do_launch_http(IP, Port, URL, RInterval, StartAfter, RNum, DestList, Nth),
    {next_state, launcher, State#launcher_http{count = Count - RNum, fraction = NewFrac2, round = Round + 1, nth = NewNth}}.

do_launch_http(_, _, _, _, _, Num, _, Nth) when Num =< 0 ->
    Nth;
do_launch_http(SrcIP, Port, URL, RInterval, StartAfter, Num, DestList, Nth) ->
    DestIP = element(Nth, DestList),
    dctg_client_sup:start_child({SrcIP, DestIP, Port, URL, RInterval, StartAfter}),
    Size = size(DestList),
    NewNth = (Nth rem Size) + 1,
    do_launch_http(SrcIP, Port, URL, RInterval, StartAfter, Num - 1, DestList, NewNth).

waitraw({launch, StartTime}, State) ->
    Time = case utils:timediff(StartTime, os:timestamp()) of
                Num when Num < 0 ->
                    0;
                Else ->
                    erlang:trunc(Else)
            end,
    gen_fsm:send_event_after(Time, {launch}),
    dctg_stat_cache:start_send(StartTime),
    {next_state, launchraw, State#launcher_raw{start_time = StartTime}}.

launchraw({launch}, State=#launcher_raw{count = Count}) when Count =:= 0 ->
    {ok, ControllerNode} = application:get_env(dctg_worker, controller),
    dctg_config_server:finish(ControllerNode),
    {stop, normal, State};
launchraw({launch}, State=#launcher_raw{
                                    intensity = Intensity,
                                    count = Count,
                                    dest = DestList,
                                    interval = Interval,
                                    sock = Sock,
                                    src_mac = SrcMac,
                                    data = Data,
                                    datalen = DataLen,
                                    start_time = StartTime,
                                    fraction = Frac,
                                    round = Round,
                                    nth = Nth
                                    }) ->
    if
        Round =:= 0 ->
            {ok, ControllerNode} = application:get_env(dctg_worker, controller),
            dctg_config_server:start_send(ControllerNode);
        true ->
            ok
    end,
    %error_logger:info_msg("WJY: launch, Count: ~p~n", [Count]),
    CurrentTime = os:timestamp(), % TODO: should be erlang:now()?
    TimePast = utils:timediff(StartTime, CurrentTime),
    Timer = case TimePast + Interval * (Round + 1) of
                Num when Num < 0 ->
                    %error_logger:info_msg("WJY: launcher: too high load!~n"),
                    0;
                Else ->
                    Else
            end,
    gen_fsm:send_event_after(erlang:round(Timer), {launch}),
    NewIntensity = erlang:trunc(Intensity),
    NewFrac = Frac + Intensity - NewIntensity,
    if
        NewFrac > 1 ->
            NewFrac2 = NewFrac - 1,
            NewIntensity2 = NewIntensity + 1;
        true ->
            NewFrac2 = NewFrac,
            NewIntensity2 = NewIntensity
    end,
    if
        Count < 0 ->
            RNum = NewIntensity2;
        true ->
            RNum = erlang:min(NewIntensity2, Count)
    end,
    NewNth = do_launch_raw(SrcMac, Data, DataLen, Sock, RNum, DestList, Nth),
    {next_state, launchraw, State#launcher_raw{count = Count - RNum, fraction = NewFrac2, round = Round + 1, nth = NewNth}}.

do_launch_raw(_, _, _, _, Num, _, Nth) when Num =< 0 ->
    Nth;
do_launch_raw(SrcMac, Data, DataLen, Sock, Num, DestList, Nth) ->
    DstMac = element(Nth, DestList),
    case procket:sendto(Sock, send_raw_packet:make_rawpkt(SrcMac, DstMac, Data)) of
        ok ->
            dctg_stat_cache:put(packet, 1),
            dctg_stat_cache:put(byte, DataLen);
        Error ->
            % error_logger:info_msg("launcher raw sendto failed, ~p~n", [Error])
            ok
    end,
    Size = size(DestList),
    NewNth = (Nth rem Size) + 1,
    do_launch_raw(SrcMac, Data, DataLen, Sock, Num - 1, DestList, NewNth).

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    %error_logger:info_msg("WJY: launcher: test sup active: ~p~n", [dctg_client_sup:active()]),
    ok.

code_change(_Old, StateName, State, _Extra) ->
    {ok, StateName, State}.

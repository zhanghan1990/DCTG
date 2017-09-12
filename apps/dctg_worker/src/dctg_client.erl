-module(dctg_client).

-behaviour(gen_fsm).

-export([start/1]).

-export([init/1, handle_event/3, handle_sync_event/4, tcpconn/2,
        %waitrecv/2,
        handle_info/3, terminate/3, code_change/4]).

-include("dctg_record.hrl").

-record(state, {
    src,
    dst,
    port,
    url,
    interval,
    start_after,
    sock,
    start_time,
    round = 0
    }).

start(Args) ->
    %error_logger:info_msg("WJY: start client args ~p~n", [Args]),
    gen_fsm:start_link(?MODULE, Args, []).

init({SrcIP, DestIP, Port, URL, Interval, StartAfter}) ->
    %dctg_stat_cache:put(init, 1),
    {ok, tcpconn, #state{src = SrcIP, dst = DestIP,
                        port = Port, url = URL,
                        interval = Interval,
                        start_after = StartAfter}, 0}.

tcpconn(timeout, State = #state{
                        src = SrcIP,
                        dst = DestIP,
                        port = Port,
                        url = URL,
                        interval = Interval,
                        start_after = StartAfter,
                        sock = Sock}) ->
    case Sock of
        undefined ->
            StartTime = os:timestamp(),
            %dctg_stat_cache:put(tcpconn, 1),
            NewSock = connect(SrcIP, DestIP, Port),
            case Interval of
                0 ->
                    %send(NewSock, URL),
                    {next_state, tcpconn, State#state{sock = NewSock}};
                _ ->
                    %send(NewSock, URL),
                    CurrentTime = os:timestamp(),
                    {T1, T2, T3} = StartTime,
                    NewStartTime = {T1, T2 + StartAfter / 1000, T3},
                    TimePast = utils:timediff(NewStartTime, CurrentTime),
                    Timer = case TimePast of
                        Num when Num < 0 ->
                            0;
                        Else ->
                            Else
                    end,
                    gen_fsm:send_event_after(round(Timer), timeout),
                    {next_state, tcpconn, State#state{sock = NewSock, start_time = NewStartTime}}
            end;
        _ ->
            send(Sock, URL),
            CurrentTime = os:timestamp(),
            StartTime = State#state.start_time,
            Round = State#state.round,
            TimePast = utils:timediff(StartTime, CurrentTime),
            Timer = case TimePast + Interval * (Round + 1) of
                        Num when Num < 0 ->
                            0;
                        Else ->
                            Else
                    end,
            gen_fsm:send_event_after(round(Timer), timeout),
            {next_state, tcpconn, State#state{round = Round + 1}}
    end.

% waitrecv(_, State) ->
%     {next_state, waitrecv, State}.

connect(SrcIP, DestIP, Port) ->
    case catch gen_tcp:connect(DestIP, Port, [{ip, SrcIP},
                                        {active, 10},
                                        {keepalive, true} % WJYTODO
                                        ]) of
        {ok, Sock} ->
            dctg_stat_cache:put(connect, 1),
            Sock;
        {error, Reason} ->
            error_logger:info_msg("WJY: client tcp connect fail, ~p~n", [Reason]),
            exit(failed);
        Exit ->
            error_logger:info_msg("WJY: client tcp connect error ~p~n", [Exit]),
            exit(failed)
    end.

send(Sock, Cont) ->
    case gen_tcp:send(Sock, Cont) of
        ok ->
            dctg_stat_cache:put(request, 1);
        {error, Reason} ->
            error_logger:info_msg("WJY: client tcp send fail ~p~n", [Reason])
    end.

handle_event(_Ev, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Ev, _From, StateName, State) ->
    {next_state, StateName, State}.

% handle_info(_Info, waitrecv, #state{sock = Sock}) ->
%     gen_tcp:close(Sock),
%     {stop, normal, ok};
handle_info({tcp_passive, _S}, StateName, State = #state{sock = Sock}) ->
    inet:setopts(Sock, [{active, 10}]),
    {next_state, StateName, State};
handle_info(_Info, StateName, State) ->
    %error_logger:info_msg("WJY: received: ~p, time: ~p~n", [Info, os:timestamp()]),
    %erlang:garbage_collect(self()),%or hibernate?
    {next_state, StateName, State, hibernate}.

terminate(_Reason, _StateName, #state{sock = Sock}) ->
    error_logger:info_msg("WJY: client terminate!!!~n"),
    if
        Sock /= undefined ->
            gen_tcp:close(Sock);
        true ->
            ok
    end,
    ok.

code_change(_Old, StateName, State, _Extra) ->
    {ok, StateName, State}.

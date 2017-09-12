-module(dctg_test).

-export([run/0, raw/2]).

run() ->
    Hosts = [
            tester1,
            tester2,
            tester3,
            tester4
    ],
    IPPropList = [{tester1, ["10.0.0.3", "10.0.0.4", "10.0.0.5"]},
                {tester2, ["10.0.0.6", "10.0.0.7", "10.0.0.8"]},
                {tester3, ["10.0.0.9", "10.0.0.10", "10.0.0.11"]},
                {tester4, ["10.0.0.12", "10.0.0.13", "10.0.0.14"]}
    ],
    DutStartIP = "10.1.0.1",
    DutNum = 8,
    Type = http,
    Intensity = 80000, % conn/s
    ConnCount = 700000,

    LauncherNum = 12,
    Port = 80,
    Content = "/a.html",
    Interval = 0,
    NumPerIP = 1, 
    dctg_frontend:set_hostip(Hosts, IPPropList),
    dctg_frontend:total(LauncherNum),
    dctg_frontend:config(DutStartIP, DutNum, Type, Intensity, ConnCount, LauncherNum, Port, Content, Interval, NumPerIP),
    dctg_controller:start_launchers().

raw(Type, IP) ->
    SrcDev = "eth0",
%    SrcMac = <<250,22,62,37,187,51>>,
%    DstMac = <<250,22,62,225,91,175>>,
    SrcMac = send_raw_packet:get_src_mac(SrcDev),
    Path = procket_mktmp:name("/tmp/procket_sock_XXXXXXXXXXXX"),
    {ok, Socket} = procket:open(0, [{protocol, procket:ntohs(16#0003)},
                                    {family, packet}, {type, raw},
                                    {pipe, Path}]),
    Ifindex = packet:ifindex(Socket, SrcDev),
    ok = packet:bind(Socket, Ifindex),
    DstMac = send_raw_packet:get_ip_by_ping(IP),

    SrcMac1 = <<250,22,62,37,188,52>>,

    case Type of 
    0 -> Pkt = send_raw_packet:make_rawpkt(SrcMac, DstMac, <<16#08, 16#00, 16#ff, 16#ee>>);
    1 -> Pkt = send_raw_packet:make_rawpkt(SrcMac, DstMac, <<16#13, 16#14, 16#ff, 16#ee>>);
    2 -> Pkt = send_raw_packet:make_rawippkt(SrcMac, DstMac, {10,0,0,2}, {10,0,0,3}, <<16#ff, 16#ee>>);
    3 -> Pkt = send_raw_packet:make_rawippkt(SrcMac, DstMac, {10,0,0,2}, {10,0,0,3}, <<16#08, 16#00>>);
    4 -> Pkt = send_raw_packet:make_rawspecialippkt(SrcMac, DstMac, {10,0,0,2}, {10,0,0,3});
    5 -> Pkt = send_raw_packet:make_rawspecialippkt(SrcMac1, DstMac, {10,0,0,2}, {10,0,0,3});
    6 -> Pkt = send_raw_packet:make_arp(1, SrcMac, {10,0,0,2}, {10,0,0,3});
    7 -> Pkt = send_raw_packet:make_arp(1, SrcMac1, {10,0,0,2}, {10,0,0,3})
    end,
    {S1, S2, S3} = os:timestamp(),
    limitsend(Socket, Pkt, 2000, 10, 500),
    {E1, E2, E3} = os:timestamp(),
    Time = (E1 - S1) * 1000000000 + (E2 - S2) * 1000 + (E3 - S3) / 1000,
    error_logger:info_msg("1000000 pkt use ~p ms~n", [Time]),
    1000 / Time.

limitsend(_, _, _, _, 0) ->
    ok;
limitsend(Socket, Pkt, Num, Time, Round) ->
    erlang:send_after(Time, self(), ok),
    send(Socket, Pkt, Num),
    receive
        ok ->
            limitsend(Socket, Pkt, Num, Time, Round - 1)
    end.

send(_, _, 0) ->
    ok;
send(Socket, Pkt, Num) ->
    procket:sendto(Socket, Pkt),
    send(Socket, Pkt, Num - 1).
-record(launcher_http,
    {
    ip, % src ip
    intensity, % launch client per ms
    count, % total count of clients
    dest,
    interval, % scheduled time interval
    port,
    url,
    req_interval,
    start_after,
    start_time,
    fraction,
    round,
    nth % the next dest ip used is the nth of dest list
    }).

-record(launcher_raw,
{
    intensity, % launch client per ms
    count, % total count of clients
    dest,
    interval,
    sock,
    src_mac,
    data,
    datalen,
    start_time,
    fraction,
    round,
    nth
}).

-record(config, {
    dut = {127, 0, 0, 1}, % ip of dut
    dutnum,
    dutlist, % ip list of dut
    type, % type of test, should be raw or http
    intensity, % tcp conn per ms per launcher
    protocol % protocol specific config
    }).

-record(http, {
    port = 80,
    content, % URL
    interval, % download interval, ms
    start_time
    }).

-record(raw, {
    data,
    len
    }).
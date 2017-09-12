-record(config, {
    dut = {127, 0, 0, 1}, % ip of dut
    dutnum,
    dutlist, % ip list of dut, to ensure O(1) random access, the data structure is tuple
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
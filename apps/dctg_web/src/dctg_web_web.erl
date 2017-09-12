%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for dctg_web.

-module(dctg_web_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                error_logger:info_msg("WJY: GET: ~p~n", [Req:dump()]),
                case Path of
                    "start" ->
                        os:cmd("rm /home/ubuntu/dctg/launcher*.log"), % remove log files
                        %os:cmd("mysql -u dctg -pdctg dctg -e \'truncate table stat;\'"), % truncate mysql table
                        dctg_controller:start_launchers(),
                        Req:respond({200, [{"Content-Type", "text/plain"}], "ok"});
                    "stop" ->
                        dctg_controller:stop(),
                        Req:respond({200, [{"Content-Type", "text/plain"}], "ok"});
                    "status" ->
                        Status = dctg_controller:status(),
                        Res = case Status of
                            wait ->
                                "wait";
                            init ->
                                "setting up tester";
                            starting ->
                                "starting traffic in 10 seconds";
                            running ->
                                "sending";
                            finish ->
                                "finished";
                            stop ->
                                "stopped"
                        end,
                        Req:respond({200, [{"Content-Type", "text/plain"}, {"Access-Control-Allow-Origin", "*"}], Res});
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                Body = Req:recv_body(),
                Json = mochijson2:decode(Body),
                {struct, JsonBody} = Json,
                error_logger:info_msg("WJY: POST: ~p~n", [JsonBody]),
                BiHostList = proplists:get_value(<<"hosts">>, JsonBody),
                {struct, IPs} = proplists:get_value(<<"ips">>, JsonBody),
                Fun = fun(I, AccI) -> mapfoldfun(I, AccI, IPs) end,
                {HostList, IPPropList} = lists:mapfoldl(Fun, [], BiHostList),
                LauncherNum = calc_length(IPPropList),

                dctg_frontend:total(LauncherNum),
                dctg_frontend:set_hostip(HostList, IPPropList),

                DutStartIP = binary_to_list(proplists:get_value(<<"dutstartip">>, JsonBody)),
                DutNum = binary_to_integer(proplists:get_value(<<"dutnum">>, JsonBody)),
                Intensity = binary_to_integer(proplists:get_value(<<"intensity">>, JsonBody)),
                Connection = binary_to_integer(proplists:get_value(<<"connection">>, JsonBody)),
                %error_logger:info_msg("WJY: POST: ~p ~p ~p~n", [DutNum, Intensity, Connection]),
                BiType = proplists:get_value(<<"type">>, JsonBody),
                Type = binary_to_atom(BiType, utf8),
                {struct, Content} = proplists:get_value(BiType, JsonBody),
                case Type of
                    http ->
                        Port = case proplists:get_value(<<"port">>, Content) of
                            undefined ->
                                80;
                            Value ->
                                binary_to_integer(Value)
                        end,
                        URL = binary_to_list(proplists:get_value(<<"url">>, Content)),
                        Interval = binary_to_integer(proplists:get_value(<<"interval">>, Content)),
                        dctg_frontend:config(DutStartIP, DutNum, Type, Intensity, Connection, LauncherNum, Port, URL, Interval);
                    raw ->
                        Data = binary_to_list(proplists:get_value(<<"data">>, Content)),
                        Length = case proplists:get_value(<<"length">>, Content) of
                            undefined ->
                                undefined;
                            Value ->
                                binary_to_integer(Value)
                        end,
                        case proplists:get_value(<<"numperip">>, JsonBody) of
                            undefined ->
                                dctg_frontend:config(DutStartIP, DutNum, Type, Intensity, Connection, LauncherNum, Data, Length);
                            Other ->
                                NumperIP = binary_to_integer(Other),
                                dctg_frontend:config(DutStartIP, DutNum, Type, Intensity, Connection, LauncherNum, Data, Length, NumperIP)
                        end
                end,
                Req:respond({200, [{"Content-Type", "text/plain"}], "ok"});
            _ ->
                Req:respond({501, [], []})
        end
    catch
        TypeWhat ->
            Report = ["web request failed",
                      {path, Path},
                      {typewhat, TypeWhat},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

mapfoldfun(Item, AccIn, IPs) ->
    Item2 = binary_to_atom(Item, utf8),
    List = getiplist(Item, IPs),
    PropList = [{Item2, List}],
    AccOut = lists:append(AccIn, PropList),
    {Item2, AccOut}.
getiplist(BiHostName, IPs) ->
    BiIPList = proplists:get_value(BiHostName, IPs),
    lists:map(fun(I) -> binary_to_list(I) end, BiIPList).

foldfun({_, List}, In) ->
    In + length(List).
calc_length(IPPropList) ->
    Fun = fun(I, In) -> foldfun(I, In) end,
    lists:foldl(Fun, 0, IPPropList).

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.

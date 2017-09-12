-module(dctg_start_launcher).

-behaviour(gen_server).

-export([start_link/0, newbeams/0, launch_start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(START_DELAY, 10).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

newbeams() ->
    gen_server:cast(?MODULE, {newbeams}).

launch_start() ->
    gen_server:cast(?MODULE, {launch_start}).

stop() ->
    gen_server:cast(?MODULE, {stop}).

init([]) ->
    {ok, ok}.

handle_cast({newbeams}, _State) ->
    HostList = dctg_config_server:get_hostlist(),
    {ok, File} = file:open("sysarg.config", [read]),
    {ok, FileArg} = file:read_line(File),
    FileArg1 = string:strip(FileArg, both, $\n),
    SysArgs = FileArg1 ++ " -rsh ssh -detached -hidden -setcookie " ++ atom_to_list(erlang:get_cookie()),
    %other args: -boot xxx -boot_var path/xxx  +A 16 -kernel xxxxx
    {ok, PAList} = init:get_argument(pa),
    PA = lists:flatten(lists:flatmap(fun(A) -> [" -pa "] ++ A end, PAList)),
    Args = SysArgs ++ " -s dctg startworker -dctg_worker controller "
        ++ atom_to_list(node()) ++ PA,
    error_logger:info_msg("Args: ~p~n", [Args]),
    {HostIDList, LauncherNum} = lists:mapfoldl(fun(Host, Acc) -> {{Host, Acc}, Acc + 1}end, 0, HostList),
    dctg_monitor:set_launchernum(LauncherNum),
    Fun = fun({Host, ID}) -> remote_launcher(Host, ID, Args) end,
    RemoteNodes = utils:pmap(Fun, HostIDList),
    {noreply, RemoteNodes};

handle_cast({launch_start}, RemoteNodes) ->
    {T1, T2, T3} = os:timestamp(),
    StartTime = {T1, T2 + ?START_DELAY, T3}, % WJY hardcoded start time, START_DELAYs after all launcher started
    error_logger:info_msg("WJY: start time: ~p~n, Nodes: ~p~n", [StartTime, RemoteNodes]),
    StartLaunchers = fun(Node) -> dctg_launcher:launch({Node, StartTime}) end,
    lists:foreach(StartLaunchers, RemoteNodes),
    {noreply, RemoteNodes};

handle_cast({stop}, State) ->
    if
        is_list(State) ->
            lists:foreach(fun(Node) -> slave:stop(Node) end, State),
            {noreply, ok};
        true ->
            {noreply, State}
    end.

remote_launcher(Host, ID, Args) ->
    Name = list_to_atom("launcher" ++ integer_to_list(ID)),
    case slave:start(Host, Name, Args) of
        {ok, Node} ->
            case net_kernel:connect_node(Node) of
                true ->
                    % error_logger:info_msg("WJY: connect_node OK~n");
                    ok;
                _Else ->
                    error_logger:info_msg("WJY: connect_node fail~n")
            end,
            Node;
        {error, Reason} ->
            error_logger:info_msg("WJY: start slave failed: ~p~n", [Reason]),
            exit({slave_fail, Reason})
    end.

code_change(_Old, State, _Extra) ->
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

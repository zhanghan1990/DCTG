-module(dctg_client_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1, active/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
    %error_logger:info_msg("WJY: client sup start child args ~p~n", [Args]),
    supervisor:start_child(?MODULE, [Args]).

active() ->
    %error_logger:info_msg("WJY: client sup: active~n"),
    PropList = supervisor:count_children(?MODULE),
    %error_logger:info_msg("WJY: client sup: count_children: ~p~n", [PropList]),
    proplists:get_value(active, PropList, error).

init([]) ->
    %error_logger:info_msg("WJY: client sup init~n"),
    SupFlags = {simple_one_for_one, 1, 2000},
    ChildSpec = [{dctg_client, {dctg_client, start, []}, temporary, 2000, worker, [dctg_client]}],
    {ok, {SupFlags, ChildSpec}}.
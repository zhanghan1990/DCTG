-module(dctg_client_killer).

-behaviour(gen_server).

-export([start_link/0, kill_finish/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TIMEOUT, 1000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

kill_finish() ->
    gen_server:cast(?MODULE, kill_finish).

init([]) ->
    {ok, ok}.

handle_cast(kill_finish, State) ->
    %error_logger:info_msg("WJY: client killer: start check~n"),
    check_active(State).

handle_call(_, _, State) ->
    {reply, error, State}.

handle_info({timeout, _, kill_finish}, State) ->
    check_active(State).

check_active(State) ->
    %error_logger:info_msg("WJY: WTF???????~n"),
    case dctg_client_sup:active() of
        Num when is_number(Num) ->
            error_logger:info_msg("WJY: client killer: ~p remain~n", [Num]),
            if
                Num =< 0 ->
                    error_logger:info_msg("WJY: client killer: kill ~n"),
                    {stop, normal, State};
                true ->
                    erlang:start_timer(?TIMEOUT, self(), kill_finish),
                    {noreply, State}
            end;
        _Other ->
            error_logger:info_msg("WJY: client killer: error~n"),
            {noreply, State}
    end.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    error_logger:info_msg("WJY: killer terminate~n"),
    {ok, ControllerNode} = application:get_env(dctg_worker, controller),
    dctg_config_server:finish(ControllerNode),
    %slave:stop(node()),
    ok.
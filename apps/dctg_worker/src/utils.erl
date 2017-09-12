-module(utils).

-export([pmap/2, timediff/2, get_id/0]).

pmap(F, L) ->
    Parent = self(),
    [receive {Pid, Result} -> Result end || Pid <- [spawn(fun() -> Parent ! {self(), F(X)} end) || X <- L]].

timediff({TA1, TA2, TA3}, {TB1, TB2, TB3}) ->
    TA = TA1 * 1000000000 + TA2 * 1000 + TA3 / 1000,
    TB = TB1 * 1000000000 + TB2 * 1000 + TB3 / 1000,
    TA - TB.

get_id() ->
    ["launcher" ++ I, _] = string:tokens(atom_to_list(node()), "@"),
    list_to_integer(I).
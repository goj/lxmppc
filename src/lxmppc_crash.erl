-module(lxmppc_crash).
-compile(export_all).

-define(TAG, <<"<test attr=\"val\">abc</test>">>).

crash() ->
    {ok, P} = exml_event:new_parser(),
    exml_event:parse(P, <<"<stream>">>),
    crash(1000, P, []).

crash(0, _, Acc) ->
    Acc;
crash(N, P, _) when N rem 100 == 0 ->
    erlang:garbage_collect(),
    crash(N - 1, P, []);
crash(N, P, Acc) ->
    crash(N - 1, P, [exml_event:parse(P, ?TAG)|Acc]).

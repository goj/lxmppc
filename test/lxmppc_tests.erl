%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Tests for lxmppc application
%%%
%%% @end
%%%===================================================================

-module(lxmppc_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").
-include("lxmppc.hrl").

-compile(export_all).

application_test() ->
    ?assertEqual(ok, application:start(lxmppc)),
    ?assertEqual(ok, application:stop(lxmppc)).

auth_plain_stanzas_test() ->
    Username = <<"romeo">>,
    Password = <<"I <3 Juliet">>,
    Stanza = lxmppc_stanza:auth_plain(Username, Password),
    ExpectedAttrs = [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-sasl">>}],
    ?assertEqual(<<"auth">>, Stanza#xmlelement.name),
    ?assertEqual(ExpectedAttrs, Stanza#xmlelement.attrs),
    [CData] = Stanza#xmlelement.body,
    Payload = base64:decode(exml:unescape_cdata(CData)),
    ExpectedAuth = <<Username/binary, 0:8, Password/binary, 0:8>>,
    ?assertEqual(ExpectedAuth, Payload#xmlcdata.content).

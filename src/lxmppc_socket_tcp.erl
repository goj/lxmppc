%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Module abstracting TCP connection to XMPP server
%%% @end
%%%===================================================================
-module(lxmppc_socket_tcp).

%% API exports
-export([connect/1, stop/1]).

-include("lxmppc.hrl").

-record(state, {owner, socket, parser}).

-define(CONNECT_TIMEOUT, 5000).

%%%===================================================================
%%% API exports
%%%===================================================================

-spec connect({binary(), integer()}) -> {ok, #transport{}}.
connect({Host, Port}) ->
    Owner = self(),
    {Pid, Mon} = erlang:spawn_monitor(fun() ->
        HostStr = binary_to_list(Host),
        Opts = [binary, {active, once}],
        {ok, Socket} = gen_tcp:connect(HostStr, Port, Opts),
        {ok, Parser} = exml_stream:new_parser(),
        Owner ! {connected, self(), Socket},
        loop(#state{owner = Owner,
                    socket = Socket,
                    parser = Parser})
    end),
    receive
        {connected, Pid, Socket} ->
            {ok, #transport{
                    module = ?MODULE,
                    socket = Socket,
                    rcv_pid = Pid
                 }};
        {'DOWN', Mon, process, Pid, Reason} ->
            {error, {couldnt_connect, {Host, Port}, Reason}}
    after ?CONNECT_TIMEOUT ->
        {error, {couldnt_connect, {Host, Port}, timeout}}
    end.

-spec stop(#transport{}) -> ok.
stop(#transport{rcv_pid=Pid}) ->
    Pid ! stop,
    ok.

%%%===================================================================
%%% receiver implementation
%%%===================================================================

loop(#state{owner = Owner, socket = Socket, parser = Parser} = State) ->
    receive
        {tcp, Socket, Data} ->
            {ok, NewParser, Stanzas} = exml_stream:parse(Parser, Data),
            lists:foreach(fun(Stanza) ->
                Owner ! stanza_msg(Socket, Stanza)
            end, Stanzas),
            loop(State#state{parser = NewParser});
        stop ->
            exml_stream:free_parser(Parser);
        Other ->
            %% TODO: implement
            error_logger:info_msg("FIXME: unhandled message", [Other]),
            exit({bad_msg, Other})
    end.

stanza_msg(Socket, Stanza) ->
    Transport = #transport{module = ?MODULE,
                           socket = Socket,
                           rcv_pid = self()},
    {stanza, Transport, Stanza}.

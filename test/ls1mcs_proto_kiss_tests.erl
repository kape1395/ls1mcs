-module(ls1mcs_proto_kiss_tests).
-behavour(ls1mcs_protocol).
-export([send/2, received/2]).
-include_lib("eunit/include/eunit.hrl").


%%
%%
%%
send_test() ->
    application:start(gproc),
    gproc:reg({n, l, {send_test, lower}}),
    gproc:reg({n, l, {send_test, upper}}),
    {ok, _PID} = ls1mcs_proto_kiss:start_link(
        {n, l, send_test},
        ls1mcs_protocol:make_ref(?MODULE, {n, l, {send_test, lower}}),
        ls1mcs_protocol:make_ref(?MODULE, {n, l, {send_test, upper}})
    ),
    send_assert(<<1, 2,     3>>, <<16#C0, 0, 1, 2,            3, 16#C0>>),
    send_assert(<<1, 16#C0, 3>>, <<16#C0, 0, 1, 16#DB, 16#DC, 3, 16#C0>>),
    send_assert(<<1, 16#DB, 3>>, <<16#C0, 0, 1, 16#DB, 16#DD, 3, 16#C0>>),
    ok.

send_assert(Input, Output) ->
    ok = ls1mcs_proto_kiss:send({n, l, send_test}, Input),
    ok = receive
        {send, Output} -> ok
        after 200 -> fail
    end.


%%
%%
%%
received_test() ->
    application:start(gproc),
    gproc:reg({n, l, {received_test, lower}}),
    gproc:reg({n, l, {received_test, upper}}),
    {ok, _PID} = ls1mcs_proto_kiss:start_link(
        {n, l, received_test},
        ls1mcs_protocol:make_ref(?MODULE, {n, l, {received_test, lower}}),
        ls1mcs_protocol:make_ref(?MODULE, {n, l, {received_test, upper}})
    ),
    received_assert(<<12, 2, 16#C0, 0, 1, 2,            3, 16#C0, 1, 2>>, <<1, 2,     3>>),
    received_assert(<<1, 2, 16#C0, 0, 1, 16#DB, 16#DC, 3, 16#C0, 1, 2>>, <<1, 16#C0, 3>>),
    received_assert(<<1, 2, 16#C0, 0, 1, 16#DB, 16#DD, 3, 16#C0, 1, 2>>, <<1, 16#DB, 3>>),
    received_assert(<<1, 2, 16#C0, 0, 1, 16#DB,        3, 16#C0, 1, 2>>, <<1, 3>>), % bad escape
    ok.

received_assert(Input, Output) ->
    ok = ls1mcs_proto_kiss:received({n, l, received_test}, Input),
    ok = receive
        {received, Output} -> ok
        after 200 -> fail
    end.


%%
%%  Helper functions, implementation for ls1mcs_protocol.
%%
send(Ref, Data) ->
    io:format("send: ~p~n", [Data]),
    gproc:send(Ref, {send, Data}),
    ok.

received(Ref, Data) ->
    io:format("received: ~p~n", [Data]),
    gproc:send(Ref, {received, Data}),
    ok.


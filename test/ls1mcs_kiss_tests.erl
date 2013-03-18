-module(ls1mcs_kiss_tests).
-behavour(ls1mcs_protocol).
-export([send/2, received/2]).
-include_lib("eunit/include/eunit.hrl").

%%
%%
%%
send_test() ->
    {ok, _PID} = ls1mcs_kiss:start_link(
        {local, send_test},
        ls1mcs_protocol:make_ref(?MODULE, {lower, self()}),
        ls1mcs_protocol:make_ref(?MODULE, {upper, self()})
    ),
    send_assert(<<1, 2,     3>>, <<16#C0, 0, 1, 2,            3, 16#C0>>),
    send_assert(<<1, 16#C0, 3>>, <<16#C0, 0, 1, 16#DB, 16#DC, 3, 16#C0>>),
    send_assert(<<1, 16#DB, 3>>, <<16#C0, 0, 1, 16#DB, 16#DD, 3, 16#C0>>).

send_assert(Input, Output) ->
    ok = ls1mcs_kiss:send(send_test, Input),
    ok = receive
        {send, Output} -> ok
        after 200 -> fail
    end.


%%
%%
%%
received_test() ->
    {ok, _PID} = ls1mcs_kiss:start_link(
        {local, received_test},
        ls1mcs_protocol:make_ref(?MODULE, {lower, self()}),
        ls1mcs_protocol:make_ref(?MODULE, {upper, self()})
    ),
    received_assert(<<1, 2, 16#C0, 0, 1, 2,            3, 16#C0, 1, 2>>, <<1, 2,     3>>),
    received_assert(<<1, 2, 16#C0, 0, 1, 16#DB, 16#DC, 3, 16#C0, 1, 2>>, <<1, 16#C0, 3>>),
    received_assert(<<1, 2, 16#C0, 0, 1, 16#DB, 16#DD, 3, 16#C0, 1, 2>>, <<1, 16#DB, 3>>),
    received_assert(<<1, 2, 16#C0, 0, 1, 16#DB,        3, 16#C0, 1, 2>>, <<1, 3>>). % bad escape

received_assert(Input, Output) ->
    ok = ls1mcs_kiss:received(received_test, Input),
    ok = receive
        {received, Output} -> ok
        after 200 -> fail
    end.


%%
%%  Helper functions, implementation for ls1mcs_protocol.
%%
send({lower, TestPID}, Data) ->
    io:format("send: ~p~n", [Data]),
    TestPID ! {send, Data},
    ok.

received({upper, TestPID}, Data) ->
    io:format("received: ~p~n", [Data]),
    TestPID ! {received, Data},
    ok.


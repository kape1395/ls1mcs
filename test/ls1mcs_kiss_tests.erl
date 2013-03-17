-module(ls1mcs_kiss_tests).
-behavour(ls1mcs_protocol).
-export([received/2]).
-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    <<16#C0, 0, 1, 2,            3, 16#C0>> = ls1mcs_kiss:encode(<<1, 2,     3>>),
    <<16#C0, 0, 1, 16#DB, 16#DC, 3, 16#C0>> = ls1mcs_kiss:encode(<<1, 16#C0, 3>>),
    <<16#C0, 0, 1, 16#DB, 16#DD, 3, 16#C0>> = ls1mcs_kiss:encode(<<1, 16#DB, 3>>).

decode_test() ->
    {ok, _PID} = ls1mcs_kiss:start_link(
        {local, decode_test},
        ls1mcs_protocol:make_ref(?MODULE, {some, self()})
    ),
    decode_send_assert([1, 2, 16#C0, 0, 1, 2,            3, 16#C0, 1, 2], [1, 2,     3]),
    decode_send_assert([1, 2, 16#C0, 0, 1, 16#DB, 16#DC, 3, 16#C0, 1, 2], [1, 16#C0, 3]),
    decode_send_assert([1, 2, 16#C0, 0, 1, 16#DB, 16#DD, 3, 16#C0, 1, 2], [1, 16#DB, 3]),
    decode_send_assert([1, 2, 16#C0, 0, 1, 16#DB,        3, 16#C0, 1, 2], [1, 3]). % bad escape

decode_send_assert(Input, Output) ->
    lists:foreach(fun(Byte) -> ok = ls1mcs_kiss:decode(decode_test, Byte) end, Input),
    ok = receive
        {received, Output} -> ok
        after 200 -> fail
    end.

received({some, TestPID}, Data) ->
    io:format("received: ~p~n", [[1395 | Data]]),
    TestPID ! {received, Data},
    ok.


-module(ls1mcs_proto_ls1p_tests).
-include_lib("eunit/include/eunit.hrl").
-include("ls1p.hrl").

-define(aen(A, B), {ok, A} = ls1mcs_proto_ls1p:encode(B)).
-define(ade(A, B), {ok, A} = ls1mcs_proto_ls1p:decode(B)).

encode_test() ->
    ?aen(<<16#0104EA0000:40>>, #ls1p_cmd_frame{dest_addr = arm, dest_port = ping, ack = true, cref = 1258, delay = 0, data = <<>>}),
    ok.


decode_test() ->
    ?ade(#ls1p_dat_frame{src_addr = arm, src_port = ping, ack = true, cref = 1258, fragment = 0, data = <<>>}, <<16#0104EA0000:40>>),
    ok.


-module(ls1mcs_proto_ls1p_tests).
-include_lib("eunit/include/eunit.hrl").
-include("ls1p.hrl").

-define(aen(A, B), {ok, A} = ls1mcs_proto_ls1p:encode(B)).
-define(ade(A, B), {ok, A} = ls1mcs_proto_ls1p:decode(B)).

encode_test() ->
    ?aen(<<16#0104EA0000:40>>,           #ls1p_cmd_frame{addr = arm, port = ping,         ack = true,  cref = 1258, delay = 0, data = <<>>}),
    ?aen(<<16#0204EC000004EB:56>>,       #ls1p_cmd_frame{addr = arm, port = kill,         ack = false, cref = 1260, delay = 0, data = <<1259:16>>}),
    ?aen(<<16#0404EB0000000002001B:80>>, #ls1p_cmd_frame{addr = arm, port = downlink,     ack = false, cref = 1259, delay = 0, data = <<0:8, 2:16, 27:16>>}),
    ?aen(<<16#0604EE0000:40>>,           #ls1p_cmd_frame{addr = arm, port = runtime_tm,   ack = false, cref = 1262, delay = 0, data = <<>>}),
   %?aen(<<16#0A04EF00000002001B:72>>,   #ls1p_cmd_frame{addr = arm, port = gps_log_bin,  ack = false, cref = 1263, delay = 0, data = <<2:16, 27:16>>}),
   %?aen(<<16#0C04F000000002001B:72>>,   #ls1p_cmd_frame{addr = arm, port = gps_log_nmea, ack = false, cref = 1264, delay = 0, data = <<2:16, 27:16>>}),
    ok.


decode_test() ->
    ?ade(#ls1p_ack_frame{status = true,  cref = 1258, recv_status = 0}, <<2#11100001:8, 16#04EA:16, 16#00:8>>),
    ?ade(#ls1p_ack_frame{status = false, cref = 1258, recv_status = 0}, <<2#11100000:8, 16#04EA:16, 16#00:8>>),
    ?ade(#ls1p_tm_frame{timestamp = 10, data = <<16#FF:8, 16#17:16>>}, <<2#11100100:8, 16#000A:16, 16#FF0017:24>>),
    ok.


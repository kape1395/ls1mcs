-module(ls1mcs_proto_ls1p_tests).
-include_lib("eunit/include/eunit.hrl").
-include("ls1p.hrl").

-define(aen(A, B), {ok, A} = ls1mcs_proto_ls1p:encode(B)).
-define(ade(A, B), {ok, A} = ls1mcs_proto_ls1p:decode(B)).

encode_test() ->
    ?aen(<<16#0104EA0000:40>>,         #ls1p_cmd_frame{dest_addr = arm, dest_port = ping,         ack = true,  cref = 1258, delay = 0, data = <<>>}),
    ?aen(<<16#0204EB00000002001B:72>>, #ls1p_cmd_frame{dest_addr = arm, dest_port = cmd_log,      ack = false, cref = 1259, delay = 0, data = <<2:16, 27:16>>}),
    ?aen(<<16#0404EC000004EB:56>>,     #ls1p_cmd_frame{dest_addr = arm, dest_port = cmd_kill,     ack = false, cref = 1260, delay = 0, data = <<1259:16>>}),
    ?aen(<<16#0604ED00000002001B:72>>, #ls1p_cmd_frame{dest_addr = arm, dest_port = tm_archive,   ack = false, cref = 1261, delay = 0, data = <<2:16, 27:16>>}),
    ?aen(<<16#0804EE0000:40>>,         #ls1p_cmd_frame{dest_addr = arm, dest_port = tm_realtime,  ack = false, cref = 1262, delay = 0, data = <<>>}),
    ?aen(<<16#0A04EF00000002001B:72>>, #ls1p_cmd_frame{dest_addr = arm, dest_port = gps_log_bin,  ack = false, cref = 1263, delay = 0, data = <<2:16, 27:16>>}),
    ?aen(<<16#0C04F000000002001B:72>>, #ls1p_cmd_frame{dest_addr = arm, dest_port = gps_log_nmea, ack = false, cref = 1264, delay = 0, data = <<2:16, 27:16>>}),
    ok.


decode_test() ->
    ?ade(#ls1p_ack_frame{src_addr = arm, src_port = ping, status = true, cref = 1258, ret_code = 0}, <<16#0104EA0000:40>>),
    ok.


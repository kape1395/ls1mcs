-module(ls1mcs_proto_ax25_tests).
-include_lib("eunit/include/eunit.hrl").
-include("ls1mcs.hrl").

start() ->
    application:start(gproc),
    {}.

stop(_) ->
    application:stop(gproc),
    ok.


%%
%%
%%
bitstuff_test() ->
    <<>>                              = ls1mcs_proto_ax25:bitstuff(<<>>),
    <<2#10000000:8>>                  = ls1mcs_proto_ax25:bitstuff(<<2#10000000:8>>),
    <<2#11000000:8>>                  = ls1mcs_proto_ax25:bitstuff(<<2#11000000:8>>),
    <<2#11100000:8>>                  = ls1mcs_proto_ax25:bitstuff(<<2#11100000:8>>),
    <<2#11110000:8>>                  = ls1mcs_proto_ax25:bitstuff(<<2#11110000:8>>),
    <<2#1111100000000000:16>>         = ls1mcs_proto_ax25:bitstuff(<<2#11111000:8>>),
    <<2#1111101000000000:16>>         = ls1mcs_proto_ax25:bitstuff(<<2#11111100:8>>),
    <<2#11101100:8>>                  = ls1mcs_proto_ax25:bitstuff(<<2#11101100:8>>),
    <<2#111110111110110000000000:24>> = ls1mcs_proto_ax25:bitstuff(<<2#1111111111110000:16>>),
    <<2#000011111011100000000000:24>> = ls1mcs_proto_ax25:bitstuff(<<2#0000111111110000:16>>),
    ok.


%%
%%
%%
bitdestuff_test() ->
    <<>>                      = ls1mcs_proto_ax25:bitdestuff(<<>>),
    <<2#10000000:8>>          = ls1mcs_proto_ax25:bitdestuff(<<2#10000000:8>>),
    <<2#11000000:8>>          = ls1mcs_proto_ax25:bitdestuff(<<2#11000000:8>>),
    <<2#11100000:8>>          = ls1mcs_proto_ax25:bitdestuff(<<2#11100000:8>>),
    <<2#11110000:8>>          = ls1mcs_proto_ax25:bitdestuff(<<2#11110000:8>>),
    <<2#11111000:8>>          = ls1mcs_proto_ax25:bitdestuff(<<2#1111100000000000:16>>),
    <<2#11111100:8>>          = ls1mcs_proto_ax25:bitdestuff(<<2#1111101000000000:16>>),
    <<2#11101100:8>>          = ls1mcs_proto_ax25:bitdestuff(<<2#11101100:8>>),
    <<2#1111111111110000:16>> = ls1mcs_proto_ax25:bitdestuff(<<2#111110111110110000000000:24>>),
    <<2#0000111111110000:16>> = ls1mcs_proto_ax25:bitdestuff(<<2#000011111011100000000000:24>>),
    ok.


%%
%%  Test cases from http://srecord.sourceforge.net/crc16-ccitt.html
%%
calculate_fcs_test() ->
    AList = <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
    16#1D0F = ls1mcs_proto_ax25:calculate_fcs(<<>>),
    16#9479 = ls1mcs_proto_ax25:calculate_fcs(<<"A">>),
    16#E5CC = ls1mcs_proto_ax25:calculate_fcs(<<"123456789">>),
    16#E938 = ls1mcs_proto_ax25:calculate_fcs(<<AList/binary, AList/binary, AList/binary, AList/binary>>).


%%
%%  Check, is encoding and decoding works symmetrically.
%%
encode_decode_test() ->
    Frame1 = #ax25_frame{
        dst = #ax25_addr{call = "LY2EN", ssid = 0},
        src = #ax25_addr{call = "LY1BVB", ssid = 0},
        data = <<1, 2, 3, 4, 5, 6, 7, 8, 9>>
    },
    Frame2 = Frame1#ax25_frame{
        data = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 16#FFFFFFFF:32>>    %% Involves bitstuffing
    },
    {ok, FrameBinary1} = ls1mcs_proto_ax25:encode(Frame1),
    {ok, FrameBinary2} = ls1mcs_proto_ax25:encode(Frame2),
    io:format("FrameBinary1: ~p~n", [FrameBinary1]),
    io:format("FrameBinary2: ~p~n", [FrameBinary2]),
    {ok, Frame1} = ls1mcs_proto_ax25:decode(FrameBinary1),
    {ok, Frame2} = ls1mcs_proto_ax25:decode(FrameBinary2),
    ok.


%%
%%
%%
split_frames_test() ->
    F = 2#01111110,
    F1 = <<1, 2, 3>>,
    F2 = <<4, 5, 6>>,
    F3 = <<7, 8, 9>>,
    {<<F:8>>, []} = ls1mcs_proto_ax25:split_frames(<<F:8, F:8, F:8>>),
    {<<F1:3/binary>>, []} = ls1mcs_proto_ax25:split_frames(<<F1/binary>>),
    {<<F:8>>, [
        <<F:8, F1:3/binary, F:8>>,
        <<F:8, F2:3/binary, F:8>>
    ]} = ls1mcs_proto_ax25:split_frames(<<F:8, F1/binary, F:8, F2/binary, F:8>>),
    {<<F:8, F3/binary>>, [
        <<F:8, F1:3/binary, F:8>>,
        <<F:8, F2:3/binary, F:8>>
    ]} = ls1mcs_proto_ax25:split_frames(<<F:8, F1/binary, F:8, F2/binary, F:8, F3/binary>>),
    {<<F:8, F3/binary>>, [
        <<F1:3/binary, F:8>>,
        <<F:8, F2:3/binary, F:8>>
    ]} = ls1mcs_proto_ax25:split_frames(<<F1/binary, F:8, F2/binary, F:8, F3/binary>>),
    ok.


%%
%%
%%
parse_call_test() ->
    #ax25_addr{call = "LY2EN",  ssid =  0} = ls1mcs_proto_ax25:parse_call("LY2EN"),
    #ax25_addr{call = "LY2EN",  ssid =  0} = ls1mcs_proto_ax25:parse_call("LY2EN-0"),
    #ax25_addr{call = "LY1BVB", ssid =  1} = ls1mcs_proto_ax25:parse_call("LY1BVB-1"),
    #ax25_addr{call = "LY2EN",  ssid = 15} = ls1mcs_proto_ax25:parse_call("LY2EN-15"),
    #ax25_addr{call = "LY2EN",  ssid = 12} = ls1mcs_proto_ax25:parse_call(#ax25_addr{call = "LY2EN",  ssid = 12}),
    ok.



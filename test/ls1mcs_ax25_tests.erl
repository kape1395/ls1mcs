-module(ls1mcs_ax25_tests).
-include_lib("eunit/include/eunit.hrl").
-include("ls1mcs.hrl").


bitstuff_test() ->
    <<>>                              = ls1mcs_ax25:bitstuff(<<>>),
    <<2#10000000:8>>                  = ls1mcs_ax25:bitstuff(<<2#10000000:8>>),
    <<2#11000000:8>>                  = ls1mcs_ax25:bitstuff(<<2#11000000:8>>),
    <<2#11100000:8>>                  = ls1mcs_ax25:bitstuff(<<2#11100000:8>>),
    <<2#11110000:8>>                  = ls1mcs_ax25:bitstuff(<<2#11110000:8>>),
    <<2#1111100000000000:16>>         = ls1mcs_ax25:bitstuff(<<2#11111000:8>>),
    <<2#1111101000000000:16>>         = ls1mcs_ax25:bitstuff(<<2#11111100:8>>),
    <<2#11101100:8>>                  = ls1mcs_ax25:bitstuff(<<2#11101100:8>>),
    <<2#111110111110110000000000:24>> = ls1mcs_ax25:bitstuff(<<2#1111111111110000:16>>),
    <<2#000011111011100000000000:24>> = ls1mcs_ax25:bitstuff(<<2#0000111111110000:16>>),
    ok.


bitdestuff_test() ->
    <<>>                      = ls1mcs_ax25:bitdestuff(<<>>),
    <<2#10000000:8>>          = ls1mcs_ax25:bitdestuff(<<2#10000000:8>>),
    <<2#11000000:8>>          = ls1mcs_ax25:bitdestuff(<<2#11000000:8>>),
    <<2#11100000:8>>          = ls1mcs_ax25:bitdestuff(<<2#11100000:8>>),
    <<2#11110000:8>>          = ls1mcs_ax25:bitdestuff(<<2#11110000:8>>),
    <<2#11111000:8>>          = ls1mcs_ax25:bitdestuff(<<2#1111100000000000:16>>),
    <<2#11111100:8>>          = ls1mcs_ax25:bitdestuff(<<2#1111101000000000:16>>),
    <<2#11101100:8>>          = ls1mcs_ax25:bitdestuff(<<2#11101100:8>>),
    <<2#1111111111110000:16>> = ls1mcs_ax25:bitdestuff(<<2#111110111110110000000000:24>>),
    <<2#0000111111110000:16>> = ls1mcs_ax25:bitdestuff(<<2#000011111011100000000000:24>>),
    ok.


%%
%%  Test cases from http://srecord.sourceforge.net/crc16-ccitt.html
%%
calculate_fcs_test() ->
    AList = <<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>,
    16#1D0F = ls1mcs_ax25:calculate_fcs(<<>>),
    16#9479 = ls1mcs_ax25:calculate_fcs(<<"A">>),
    16#E5CC = ls1mcs_ax25:calculate_fcs(<<"123456789">>),
    16#E938 = ls1mcs_ax25:calculate_fcs(<<AList/binary, AList/binary, AList/binary, AList/binary>>).


%%
%%  Check, is encoding and decoding works symmetrically.
%%
encode_decode_test() ->
    Frame = #ax25_frame{
        dst = #ax25_addr{call = "LY2EN", ssid = 0},
        src = #ax25_addr{call = "LY1BVB", ssid = 0},
        data = <<1, 2, 3, 4, 5, 6, 7, 8, 9>>
    },
    {ok, FrameBinary} = ls1mcs_ax25:encode(Frame),
    io:format("FrameBinary: ~p~n", [FrameBinary]),
    {ok, Frame} = ls1mcs_ax25:decode(FrameBinary),
    ok.

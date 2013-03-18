-module(ls1mcs_ax25_tests).
-include_lib("eunit/include/eunit.hrl").
-include("ls1mcs.hrl").


bitstuff_test() ->
    {<<>>, 0} = ls1mcs_ax25:bitstuff(<<>>),
    {<<2#1:1>>, 0} = ls1mcs_ax25:bitstuff(<<2#1:1>>),
    {<<2#11:2>>, 0} = ls1mcs_ax25:bitstuff(<<2#11:2>>),
    {<<2#111:3>>, 0} = ls1mcs_ax25:bitstuff(<<2#111:3>>),
    {<<2#1111:4>>, 0} = ls1mcs_ax25:bitstuff(<<2#1111:4>>),
    {<<2#111110:6>>, 1} = ls1mcs_ax25:bitstuff(<<2#11111:5>>),
    {<<2#1111101:7>>, 1} = ls1mcs_ax25:bitstuff(<<2#111111:6>>),
    {<<2#11111011111011:14>>, 2} = ls1mcs_ax25:bitstuff(<<2#111111111111:12>>),
    {<<2#111011:6>>, 0} = ls1mcs_ax25:bitstuff(<<2#111011:6>>).


bitdestuff_test() ->
    {<<>>, 0} = ls1mcs_ax25:bitstuff(<<>>),
    {<<2#1:1>>, 0} = ls1mcs_ax25:bitdestuff(<<2#1:1>>),
    {<<2#11:2>>, 0} = ls1mcs_ax25:bitdestuff(<<2#11:2>>),
    {<<2#111:3>>, 0} = ls1mcs_ax25:bitdestuff(<<2#111:3>>),
    {<<2#1111:4>>, 0} = ls1mcs_ax25:bitdestuff(<<2#1111:4>>),
    {<<2#11111:5>>, 1} = ls1mcs_ax25:bitdestuff(<<2#111110:6>>),
    {<<2#111111:6>>, 1} = ls1mcs_ax25:bitdestuff(<<2#1111101:7>>),
    {<<2#111111111111:12>>, 2} = ls1mcs_ax25:bitdestuff(<<2#11111011111011:14>>),
    {<<2#111011:6>>, 0} = ls1mcs_ax25:bitdestuff(<<2#111011:6>>).


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

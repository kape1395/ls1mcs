-module(ls1mcs_ax25_tests).
-include_lib("eunit/include/eunit.hrl").


bitstuff_test() ->
    <<>> = ls1mcs_ax25:bitstuff(<<>>),
    <<2#1:1>> = ls1mcs_ax25:bitstuff(<<2#1:1>>),
    <<2#11:2>> = ls1mcs_ax25:bitstuff(<<2#11:2>>),
    <<2#111:3>> = ls1mcs_ax25:bitstuff(<<2#111:3>>),
    <<2#1111:4>> = ls1mcs_ax25:bitstuff(<<2#1111:4>>),
    <<2#111110:6>> = ls1mcs_ax25:bitstuff(<<2#11111:5>>),
    <<2#1111101:7>> = ls1mcs_ax25:bitstuff(<<2#111111:6>>),
    <<2#11111011111011:14>> = ls1mcs_ax25:bitstuff(<<2#111111111111:12>>),
    <<2#111011:6>> = ls1mcs_ax25:bitstuff(<<2#111011:6>>).

bitdestuff_test() ->
    <<>> = ls1mcs_ax25:bitstuff(<<>>),
    <<2#1:1>> = ls1mcs_ax25:bitdestuff(<<2#1:1>>),
    <<2#11:2>> = ls1mcs_ax25:bitdestuff(<<2#11:2>>),
    <<2#111:3>> = ls1mcs_ax25:bitdestuff(<<2#111:3>>),
    <<2#1111:4>> = ls1mcs_ax25:bitdestuff(<<2#1111:4>>),
    <<2#11111:5>> = ls1mcs_ax25:bitdestuff(<<2#111110:6>>),
    <<2#111111:6>> = ls1mcs_ax25:bitdestuff(<<2#1111101:7>>),
    <<2#111111111111:12>> = ls1mcs_ax25:bitdestuff(<<2#11111011111011:14>>),
    <<2#111011:6>> = ls1mcs_ax25:bitdestuff(<<2#111011:6>>).

-module(ls1mcs_utl_enc_tests).
-include_lib("eunit/include/eunit.hrl").


escaping_test() ->
    Data = <<0, 0, 1, 2, 3, 10, 13, 256>>,
    Encoded = ls1mcs_utl_enc:escaping_encode(Data),
    Data = ls1mcs_utl_enc:escaping_decode(Encoded).


base255wo13_test() ->
    Data = <<0, 0, 1, 2, 3, 10, 13, 256>>,
    Encoded = ls1mcs_utl_enc:base255wo13_encode(Data),
    Data = ls1mcs_utl_enc:base255wo13_decode(Encoded).



-module(ls1mcs_utl_cksum).
-export([checksum/2]).

%%
%%  Calculates hash'es / checksums.
%%
checksum(Data, fletcher8bit) ->
    {A, B} = lists:foldl(fun fletcher_fold/2, {0, 0}, erlang:binary_to_list(Data)),
    {ok, <<A:8, B:8>>}.



%%
%%  The 8-bit Fletcher algorithm (see RFC 1145 which describes TCP)
%%  is used to calculate the checksums for the Helium-100.
%%
fletcher_fold(Byte, {A, B}) ->
    NewA = 16#FF band (A + Byte),
    NewB = 16#FF band (B + NewA),
    {NewA, NewB}.

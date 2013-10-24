-module(ls1mcs_utl_enc).
-export([
    escaping_encode/1,
    escaping_decode/1,
    base255wo13_encode/1,
    base255wo13_decode/1
]).


%%
%%
%%
escaping_encode(<<>>) ->
    <<>>;

escaping_encode(<<10, Tail/binary>>) ->
    Encoded = escaping_encode(Tail),
    <<10, 0, Encoded/binary>>;

escaping_encode(<<13, Tail/binary>>) ->
    Encoded = escaping_encode(Tail),
    <<10, 3, Encoded/binary>>;

escaping_encode(<<Any, Tail/binary>>) ->
    Encoded = escaping_encode(Tail),
    <<Any, Encoded/binary>>.


%%
%%
%%
escaping_decode(<<>>) ->
    <<>>;

escaping_decode(<<10, Add, Tail/binary>>) ->
    Decoded = escaping_decode(Tail),
    <<(10 + Add), Decoded/binary>>;

escaping_decode(<<Any, Tail/binary>>) ->
    Decoded = escaping_decode(Tail),
    <<Any, Decoded/binary>>.


%%
%%
%%
base255wo13_encode(Data) ->
    PrefixedData = <<1, Data/binary>>,
    PrefixedSize = size(PrefixedData),
    <<Number:PrefixedSize/unsigned-unit:8>> = PrefixedData,
    Base255 = lists:reverse(to_base(255, Number)),
    Base255wo13 = [ shift_13(X) || X <- Base255],
    erlang:list_to_binary(Base255wo13).

base255wo13_decode(Data) ->
    Unshifted = [ unshift_13(X) || X <- binary_to_list(Data) ],
    Number = from_base(255, lists:reverse(Unshifted)),
    [1 | Base256] = lists:reverse(to_base(256, Number)),
    erlang:list_to_binary(Base256).


%% Uses reversed input list.
from_base(_Base, []) ->
    0;
from_base(Base, [First | Other]) ->
    First + Base * from_base(Base, Other).


%% Returns reversed list.
to_base(_Base, 0) ->
    [];
to_base(Base, Number) ->
    [Number rem Base | to_base(Base, Number div Base)].


shift_13(X) when X < 13 -> X;
shift_13(X) -> X + 1.

unshift_13(X) when X < 13 -> X;
unshift_13(X) -> X - 1.



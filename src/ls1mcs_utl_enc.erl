-module(ls1mcs_utl_enc).
-export([base255_encode/1, to_base/3]).


base255_encode(Data) ->
    %InitialSize = size(Data),
    %<<Number:InitialSize/unsigned-unit:8>> = Data,
    %{ok, Encoded} = {ok, InitialData},
    Base256 = lists:reverse(erlang:binary_to_list(Data)),
    Base255 = lists:reverse(to_base(256, 255, Base256)),
    {ok, Base255}.


%%
%%
%%
to_base(SrcBase, DstBase, Data) ->
    lists:reverse(to_base(SrcBase, DstBase, 1, 0, lists:reverse(Data))).

to_base(_SrcBase, _DstBase, _Mult, _Rem, []) ->
    [];

to_base(SrcBase, DstBase, Mult, Rem, Other) when Mult + Rem >= DstBase ->
    [
        (Mult + Rem) rem DstBase |
        to_base(SrcBase, DstBase, 1, (Rem) div DstBase, Other)
    ];

to_base(SrcBase, DstBase, Mult, Rem, [0 | Other]) ->
    to_base(SrcBase, DstBase, Mult * SrcBase, Rem, Other);

to_base(SrcBase, DstBase, Mult, Rem, [First | Other]) ->
    to_base(SrcBase, DstBase, 1, First * Mult + Rem, Other).



%to_base(SrcBase, DstBase, Mult, Rem, [Lowest | Other]) when (Lowest * Mult + Rem) >= DstBase ->
%    [
%        (Lowest * Mult + Rem) rem DstBase |
%        to_base(SrcBase, DstBase, Mult, Rem, [Lowest div DstBase | Other])
%    ];
%
%to_base(SrcBase, DstBase, Mult, Rem, [Lowest, Next | Other]) ->
%    to_base(SrcBase, DstBase, Mult, Rem, [Lowest + SrcBase * Next | Other]);
%
%to_base(_SrcBase, Mult, _DstBase, _Rem, [Lowest]) ->
%    [Lowest].




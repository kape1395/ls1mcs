-module(ls1mcs_protocol).
-export([make_ref/2, received/2]).

-callback received(Ref :: term(), Data :: term()) -> ok.


make_ref(Module, Ref) ->
    {Module, Ref}.


received({Module, Ref}, Data) ->
    Module:received(Ref, Data).

-module(ls1mcs_protocol).
-export([make_ref/2, send/2, received/2]).

%%
%%  Invoked when a packet should be sent (to the lower protocol).
%%
-callback send(Ref :: term(), Data :: term()) -> ok.

%%
%%  Invoked when a packet is received (from the lower protocol).
%%
-callback received(Ref :: term(), Data :: term()) -> ok.



%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%  Create a reference that should be later used to access the protocol impl.
%%
make_ref(Module, Ref) ->
    {Module, Ref}.


%%
%%  Send a frame out.
%%
send({Module, Ref}, Data) ->
    Module:send(Ref, Data).


%%
%%  New data received from the lower lewel protocol.
%%
received({Module, Ref}, Data) ->
    Module:received(Ref, Data).



%% =============================================================================
%%  Internal Functions.
%% =============================================================================


%%
%%  Base behaviour for ptotocol implementations.
%%
-module(ls1mcs_protocol).
-export([make_ref/2, await/1, send/2, received/2]).

-define(INIT_WAIT, 10000).


%%
%%  Invoked when a packet should be sent (to the lower protocol).
%%
-callback send(Ref :: term(), Data :: term()) -> ok.

%%
%%  Invoked when a packet is received (from the lower protocol).
%%
-callback recv(Ref :: term(), Data :: term()) -> ok.



%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%  Create a reference that should be later used to access the protocol impl.
%%
make_ref(Module, Ref) ->
    {Module, Ref}.


%%
%%  Waits for the protocol to start.
%%
await({_Module, Ref}) ->
    gproc:await(Ref, ?INIT_WAIT).



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


%/--------------------------------------------------------------------
%| Copyright 2013-2014 Karolis Petrauskas
%|
%| Licensed under the Apache License, Version 2.0 (the "License");
%| you may not use this file except in compliance with the License.
%| You may obtain a copy of the License at
%|
%|     http://www.apache.org/licenses/LICENSE-2.0
%|
%| Unless required by applicable law or agreed to in writing, software
%| distributed under the License is distributed on an "AS IS" BASIS,
%| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%| See the License for the specific language governing permissions and
%| limitations under the License.
%\--------------------------------------------------------------------

%%
%%  Base behaviour for ptotocol implementations.
%%
-module(ls1mcs_proto).
-compile([{parse_transform, lager_transform}]).
-export([make_ref/2, make_send_chain/1, make_recv_chain/1, send/2, recv/2]).
-include("ls1mcs.hrl").
-include("ls1p.hrl").


-type header() :: {Name :: atom(), Value :: atom()}.
-type frame() :: {[header()], Body :: term()}.

%% =============================================================================
%%  Callback definitions.
%% =============================================================================

%%
%%  Initialize the protocol state.
%%
-callback init(
        Args :: term()
    ) ->
    {ok, State :: term()}.


%%
%%  Invoked when a packet should be sent (to the lower protocol).
%%
-callback send(
        Frame :: frame(),
        State :: term()
    ) ->
    unknown |
    {ok, SubFrames :: [frame()], NewState :: term()}.


%%
%%  Invoked when a packet is received (from the lower protocol).
%%
-callback recv(
        Frame :: frame(),
        State :: term()
    ) ->
    unknown |
    {ok, SubFrames :: [frame()], NewState :: term()}.



%% =============================================================================
%%  Public API
%% =============================================================================

%%
%%  Create a reference that should be later used to access the protocol impl.
%%
make_ref(Module, Args) ->
    {ok, State} = Module:init(Args),
    {ok, {Module, State}}.


%%
%%  Create send chain from a list of protocol refs.
%%
make_send_chain(ProtoRefs) ->
    Chain = ProtoRefs,
    {ok, Chain}.


%%
%%  Create send chain from a list of protocol refs.
%%
make_recv_chain(ProtoRefs) ->
    Chain = ProtoRefs,
    {ok, Chain}.


%%
%%  Send a frame via the Chain.
%%
send(Ls1pCmdFrame, Chain) when is_record(Ls1pCmdFrame, ls1p_cmd_frame)->
    {ok, NewChain, EndFrames} = process_via_chain(Chain, {[], Ls1pCmdFrame}, send),
    {ok, [ D || {_H, D} <- EndFrames ], NewChain}.


%%
%%  Receive a data via the Chain.
%%
recv(Data, Chain) ->
    {ok, NewChain, Ls1pFrames} = process_via_chain(Chain, {[], Data}, recv),
    {ok, [ D || {_H, D} <- Ls1pFrames ], NewChain}.



%% =============================================================================
%%  Internal Functions.
%% =============================================================================

%%
%%  Process a message via the chain of protocols.
%%
process_via_chain([], Frame, _Function) ->
    {ok, [], [Frame]};

process_via_chain([{ProtoMod, ProtoState} | ChainTail], Frame, Function) ->
    try ProtoMod:Function(Frame, ProtoState) of
        {ok, NewFrames, NewProtoState} ->
            F = fun (SubFrame, {ok, SubChain, EndFrames}) ->
                {ok, NewSubChain, NewEndFrames} = process_via_chain(SubChain, SubFrame, Function),
                {ok, NewSubChain, [NewEndFrames | EndFrames]}
            end,
            {ok, LastSubChain, AllEndFrames} = lists:foldl(F, {ok, ChainTail, []}, NewFrames),
            {ok, [{ProtoMod, NewProtoState} | LastSubChain], lists:append(lists:reverse(AllEndFrames))};
        unknown ->
            lager:warning("Dropping unknown frame by ~p: ~p", [ProtoMod, Frame]),
            {ok, [{ProtoMod, ProtoState} | ChainTail], []}
    catch
        E:T ->
            lager:warning(
                "Dropping failed frame: module=~p, frame=~p, error=~p:~p, trace=~p",
                [ProtoMod, Frame, E, T, erlang:get_stacktrace()]
            ),
            {ok, [{ProtoMod, ProtoState} | ChainTail], []}
    end.


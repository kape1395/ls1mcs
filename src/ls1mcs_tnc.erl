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
%%  SAT Link Listener for SAT Commands. It listens for LS1P Ack and Data frames
%%  from the SAT Link and routes them to the corresponding SAT Command FSMs.
%%
-module(ls1mcs_tnc).
-behaviour(gen_event).
-compile([{parse_transform, lager_transform}]).
-export([register/2]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).
-include("ls1p.hrl").
-include("ls1mcs.hrl").

%% =============================================================================
%%  Callback definitions.
%% =============================================================================

%%
%%
%%
-callback send(Name :: term(), Frame :: #ls1p_cmd_frame{}) -> ok.



%% =============================================================================
%%  Public API.
%% =============================================================================

%%
%%  Registers this event handler to the SAT Link.
%%
register(Module, Name) ->
    ok = ls1mcs_sat_link:add_send_handler(?MODULE, {Module, Name}).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================


-record(state, {
    module,
    name
}).



%% =============================================================================
%%  Callbacks for gen_event.
%% =============================================================================

%%
%%  Initialize the event handler.
%%
init({Module, Name}) ->
    {ok, #state{module = Module, name = Name}}.


%%
%%  Handle events, published by the SAT Link.
%%
handle_event({send, Frame}, State = #state{module = Module, name = Name}) ->
    lager:debug("Sending frame ~p via ~p(~p)", [Frame, Module, Name]),
    ok = Module:send(Name, Frame),
    {ok, State}.


%%
%%  Handle calls addressed to this event handler.
%%
handle_call(_Request, State) ->
    {ok, undefined, State}.


%%
%%  Handle unknown messages.
%%
handle_info(_Info, State) ->
    {ok, State}.


%%
%%  Cleanup.
%%
terminate(_Arg, _State) ->
    ok.


%%
%%  Handle upgrades.
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% =============================================================================
%%  Internal Functions.
%% =============================================================================



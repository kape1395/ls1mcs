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
%%  SAT Link Listener for maintaining latest telemetry.
%%
-module(ls1mcs_telemetry_sll).
-behaviour(gen_event).
-compile([{parse_transform, lager_transform}]).
-export([register/0]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).
-include("ls1p.hrl").
-include("ls1mcs.hrl").


%% =============================================================================
%%  Public API.
%% =============================================================================

%%
%%  Registers this event handler to the SAT Link.
%%
register() ->
    ok = ls1mcs_sat_link:add_recv_handler(?MODULE, {}).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================


-record(state, {
}).



%% =============================================================================
%%  Callbacks for gen_event.
%% =============================================================================

%%
%%  Initialize the event handler.
%%
init({}) ->
    {ok, #state{}}.


%%
%%  Handle events, published by the SAT Link.
%%
handle_event({sent, Frame}, State) ->
    lager:debug("Ignoring sent frame: ~p", [Frame]),
    {ok, State};

handle_event({recv, Frame}, State) when is_record(Frame, ls1p_tm_frame) ->
    lager:debug("Received SAT TM: ~p", [Frame]),
    ls1mcs_telemetry:received(Frame),
    {ok, State};

handle_event({recv, Frame}, State) ->
    lager:debug("Ignoring recv frame: ~p", [Frame]),
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



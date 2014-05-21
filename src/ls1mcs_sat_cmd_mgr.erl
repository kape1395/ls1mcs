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
%%  Monitors sat link listener.
%%
-module(ls1mcs_sat_cmd_mgr).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("ls1mcs.hrl").


%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%
%%
start_link() ->
    {ok, _Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
}).


%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================


%%
%%
%%
init({}) ->
    ok = ls1mcs_sat_cmd_sll:register(),
    {ok, #state{}}.


%%
%%
%%
handle_call(_Message, _From, State) ->
    {reply, not_implemented, State}.


%%
%%
%%
handle_cast(_Message, State) ->
    {stop, not_implemented, State}.


%%
%%
%%
handle_info({gen_event_EXIT, _Handler, Reason}, State) ->
    {stop, {sll_exit, Reason}, State}.


%%
%%  Terminate a process.
%%
terminate(_Reason, _State) ->
    ok.


%%
%%
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% =============================================================================
%%  Internal Functions.
%% =============================================================================


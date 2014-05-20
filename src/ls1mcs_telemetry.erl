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
%%
%%
-module(ls1mcs_telemetry).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0, received/1, get_latest/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("ls1mcs.hrl").


%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).


%%
%%
%%
received(Frame) ->
    gen_server:cast(?MODULE, {received, Frame}).


%%
%%
%%
get_latest() ->
    gen_server:call(?MODULE, {get_latest}).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================


-record(state, {
    latest
}).



%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================


%%
%%
%%
init({}) ->
    ls1mcs_telemetry_sll:register(),
    Latest = case ls1mcs_store:get_tm(latest) of
        {error, not_found} -> undefined;
        {ok, TmFrame} -> TmFrame
    end,
    State = #state{
        latest = Latest
    },
    {ok, State}.


%%
%%
%%
handle_call({get_latest}, _From, State = #state{latest = Latest}) ->
    Reply = case Latest of
        undefined ->
            {error, not_found};
        _ ->
            {ok, Latest}
    end,
    {reply, Reply, State}.


%%
%%
%%
handle_cast({received, NewTM = #ls1p_tm_frame{recv = NewRecv}}, State = #state{latest = Latest}) ->
    case Latest of
        undefined ->
            {noreply, State#state{latest = NewTM}};
        #ls1p_tm_frame{recv = LastRecv} when NewRecv > LastRecv ->
            {noreply, State#state{latest = NewTM}};
        _ ->
            {noreply, State}
    end.


%%
%%
%%
handle_info(_Event, State) ->
    {noreply, State}.


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


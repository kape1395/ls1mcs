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
%%  User command: dlnk_arch.
%%  Downlinks missing archive data.
%%
-module(ls1mcs_usr_cmd_dlnkarch).
-behaviour(ls1mcs_usr_cmd).
-behaviour(gen_fsm).
-compile([{parse_transform, lager_transform}]).
-export([start_link/2]).
-export([sat_cmd_status/3]).
-export([starting/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-include("ls1mcs.hrl").

-define(REF(UsrCmdId), {via, gproc, {n, l, {?MODULE, UsrCmdId}}}).



%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%
%%
-spec start_link(#usr_cmd{}, #usr_cmd_spec{})
        -> {ok, pid()} | term().

start_link(UsrCmd = #usr_cmd{id = UsrCmdId}, _UsrCmdSpec) ->
    gen_fsm:start_link(?REF(UsrCmdId), ?MODULE, {UsrCmd}, []).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    id,             %% User command id (the same as in usr_cmd, for convenience).
    usr_cmd,        %% User command.
    buf_id,         %% Buffer ID to work with. This comes as a command argument.
    last_cmd_id,    %% Last sat cmd ID.
    block_size,     %% Block size in bytes used to download the photo.
    retry_count     %% Maximal number of times the photo_data command can be sent.
}).


%% =============================================================================
%%  Callbacks for ls1mcs_usr_cmd.
%% =============================================================================

%%
%%
%%
sat_cmd_status(UsrCmdId, SatCmdId, Status) ->
    ok = gen_fsm:send_event(?REF(UsrCmdId), {sat_cmd_status, SatCmdId, Status}).



%% =============================================================================
%%  Callbacks for gen_fsm.
%% =============================================================================

%%
%%
%%
init({UsrCmd = #usr_cmd{id = UsrCmdId, args = Args}}) ->
    gen_fsm:send_event(self(), start),
    BufId = ls1mcs_usr_cmd:arg_val(bufid, Args),
    StateData = #state{
        id = UsrCmdId,
        usr_cmd = UsrCmd,
        buf_id = BufId,
        block_size = 195,
        retry_count = 3
    },
    {ok, starting, StateData}.


%%
%%  FSM State: starting.
%%
starting(start, StateData) ->
    NewStateData = StateData, % TODO: send_photo_meta(StateData),
    {next_state, getting_meta, NewStateData}.



%%
%%  Other FSM callbacks.
%%
handle_event(_Event, StateName, StateData = #state{}) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info(_Event, StateName, StateData = #state{}) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.



%% =============================================================================
%%  Internal Functions.
%% =============================================================================


%%
%%
%%
% send_downlink(BlkSz, From, Till, StateData = #state{id = UsrCmdId, buf_id = BufId}) ->
%     SatCmd = #sat_cmd{
%         cmd_frame = #ls1p_cmd_frame{
%             addr = arm,
%             port = downlink,
%             ack = false,
%             data = <<BufId:8, BlkSz:8, From:16/little, Till:16/little>>
%         },
%         exp_dfc = Till - From
%     },
%     {ok, SatCmdId} = ls1mcs_usr_cmd:send_sat_cmd(?MODULE, UsrCmdId, SatCmd),
%     StateData#state{
%         last_cmd_id = SatCmdId
%     }.



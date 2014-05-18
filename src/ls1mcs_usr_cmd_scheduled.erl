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
%%  User command: scheduled.
%%  Arguments:
%%
%%    * `from`  - Execute from date (`yyyy-MM-dd HH:mm:ss` in UTC).
%%    * `till`  - Execute till date (`yyyy-MM-dd HH:mm:ss` in UTC).
%%    * `retry` - Number of seconds to retry the command after.
%%
%%  Implemented as an FSM with the following states:
%%
%%    * waiting     - waiting for the start time.
%%    * executing   - periodically sending commands.
%%    * finalizing  - Time for sending commands ended, or ack has been received
%%
%%  The following events:
%%
%%    * `start`
%%    * `stop`
%%    * `retry`
%%    * `cancel`
%%    * `sat_cmd_status`
%%    * `term`
%%
%%
-module(ls1mcs_usr_cmd_scheduled).
-behaviour(ls1mcs_usr_cmd).
-behaviour(gen_fsm).
-compile([{parse_transform, lager_transform}]).
-export([start_link/2, cancel/1, get_running/0]).
-export([sat_cmd_status/3]).
-export([waiting/2, executing/2, finalizing/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-include("ls1mcs.hrl").

-define(REF(UsrCmdId), {via, gproc, {n, l, {?MODULE, UsrCmdId}}}).
-define(TERM_DELAY, 60000).


%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%  Start execution of the scheduled user command.
%%
-spec start_link(#usr_cmd{}, #usr_cmd_spec{})
        -> {ok, pid()} | term().

start_link(UsrCmd = #usr_cmd{id = UsrCmdId}, _UsrCmdSpec) ->
    gen_fsm:start_link(?REF(UsrCmdId), ?MODULE, {UsrCmd}, []).


%%
%%  Cancel execution of the command.
%%
cancel(UsrCmdId) ->
    gen_fsm:send_event(?REF(UsrCmdId), cancel).


%%
%%  Returns list of currently running commands (Command ids).
%%
get_running() ->
    UsrCmdIds = [ UCId || {_Pid, UCId} <- gproc:lookup_values({p, l, ?MODULE})],
    {ok, UsrCmdIds}.



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    id,             %% User command id (the same as in usr_cmd, for convenience).
    usr_cmd,        %% User command.
    retry,          %% Retry period in milliseconds.
    sat_cmd_ids     %% Issued SAT command ids.
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
init({#usr_cmd{id = UsrCmdId, args = Args}}) ->
    true = gproc:reg({p, l, ?MODULE}, UsrCmdId),
    {ok, From}  = ls1mcs_usr_cmd:arg_value(from,  Args, tstamp),
    {ok, Till}  = ls1mcs_usr_cmd:arg_value(till,  Args, tstamp),
    {ok, Retry} = ls1mcs_usr_cmd:arg_value(retry, Args, integer),
    Now = os:timestamp(),
    FromDelay = timer:now_diff(From, Now) div 1000,
    TillDelay = timer:now_diff(Till, Now) div 1000,
    gen_fsm:send_event_after(FromDelay, start),
    gen_fsm:send_event_after(TillDelay, stop),
    StateData = #state{
        id = UsrCmdId,
        retry = Retry * 1000,
        sat_cmd_ids = []
    },
    {ok, waiting, StateData}.


%%
%%  FSM State: `waiting`.
%%
waiting(start, StateData) ->
    enter_executing(StateData);

waiting(stop, StateData) ->
    enter_finalizing(failed, StateData);

waiting(cancel, StateData) ->
    enter_finalizing(canceled, StateData).


%%
%%  FSM State: `executing`.
%%
executing(stop, StateData) ->
    enter_finalizing(failed, StateData);

executing(cancel, StateData) ->
    enter_finalizing(canceled, StateData);

executing(retry, StateData) ->
    enter_executing(StateData);

executing({sat_cmd_status, _SatCmdId, failed}, StateData) ->
    {next_state, executing, StateData};

executing({sat_cmd_status, _SatCmdId, completed}, StateData) ->
    enter_finalizing(completed, StateData);

executing({sat_cmd_status, SatCmdId, Status}, StateData) ->
    lager:warning(
        "Ingoring unexpected sat_cmd_status, sat_cmd_id=~p, status=~p.",
        [SatCmdId, Status]
    ),
    {next_state, executing, StateData}.


%%
%%  FSM State: `completed`.
%%
finalizing(term, StateData) ->
    {stop, normal, StateData};

finalizing(_Event, StateData) ->
    {next_state, finalizing, StateData}.


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
%%  Handle entry to the `executing` state.
%%
enter_executing(StateData = #state{id = UsrCmdId, retry = Retry, sat_cmd_ids = SatCmdIds}) ->
    SatCmd = #sat_cmd{
        cmd_frame = #ls1p_cmd_frame{
            addr = arm,
            port = ping,
            ack = true
        }
    },
    {ok, SatCmdId} = ls1mcs_usr_cmd:send_sat_cmd(?MODULE, UsrCmdId, SatCmd),
    NewStateData = StateData#state{
        sat_cmd_ids = [SatCmdId | SatCmdIds]
    },
    gen_fsm:send_event_after(Retry, retry),
    {next_state, executing, NewStateData}.


%%
%%  Handle entry to the `finalizing` state.
%%
enter_finalizing(FinalState, StateData = #state{id = UsrCmdId}) ->
    CommandStatus = case FinalState of
        failed    -> failed;
        canceled  -> failed;
        completed -> completed
    end,
    ls1mcs_usr_cmd:set_status(UsrCmdId, CommandStatus),
    gen_fsm:send_event_after(?TERM_DELAY, term),
    {next_state, finalizing, StateData}.



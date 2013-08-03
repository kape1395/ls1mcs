%%
%%  User command: simple parametrizable command.
%%  Executes single SAT command.
%%
-module(ls1mcs_usr_cmd_simple).
-behaviour(ls1mcs_usr_cmd).
-behaviour(gen_fsm).
-compile([{parse_transform, lager_transform}]).
-export([start_link/2]).
-export([sat_cmd_status/3]).
-export([starting/2, executing/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-include("ls1mcs.hrl").

-define(REF(UsrCmdId), {via, gproc, {?MODULE, UsrCmdId}}).


%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%
%%
-spec start_link(usr_cmd_id(), #sat_cmd{})
        -> {ok, pid()} | term().

start_link(UsrCmdId, SatCmd) ->
    gen_fsm:start_link(?REF(UsrCmdId), ?MODULE, {UsrCmdId, SatCmd}, []).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    usr_cmd_id  :: usr_cmd_id(),    %% User command id.
    sat_cmd     :: #sat_cmd{}       %% Sat command issued by this user command.
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
init({UsrCmdId, SatCmd}) ->
    gen_fsm:send_event(self(), start),
    {ok, starting, #state{usr_cmd_id = UsrCmdId, sat_cmd = SatCmd}}.


%%
%%  FSM State: starting.
%%
starting(start, StateData = #state{usr_cmd_id = UsrCmdId, sat_cmd = SatCmd}) ->
    {ok, SatCmdId} = ls1mcs_usr_cmd:send_sat_cmd(?MODULE, UsrCmdId, SatCmd),
    NewStateData = StateData#state{
        sat_cmd = SatCmd#sat_cmd{id = SatCmdId}
    },
    {next_state, executing, NewStateData}.


%%
%%  FSM State: executing.
%%
executing({sat_cmd_status, _SatCmdId, failed}, StateData = #state{usr_cmd_id = UsrCmdId, sat_cmd = SatCmd}) ->
    lager:debug("User command failed, usr_cmd_id=~p, sat_cmd=~p", [UsrCmdId, SatCmd]),
    {stop, normal, StateData};

executing({sat_cmd_status, _SatCmdId, completed}, StateData = #state{usr_cmd_id = UsrCmdId, sat_cmd = SatCmd}) ->
    lager:debug("User command completed, usr_cmd_id=~p, sat_cmd=~p", [UsrCmdId, SatCmd]),
    {stop, normal, StateData}.


%%
%%  Other FSM callbacks.
%%
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.



%% =============================================================================
%%  Internal Functions.
%% =============================================================================


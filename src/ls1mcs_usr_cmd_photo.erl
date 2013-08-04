%%
%%  User command: photo.
%%  Takes photo, gets its metadata and downloads it.
%%
-module(ls1mcs_usr_cmd_photo).
-behaviour(ls1mcs_usr_cmd).
-behaviour(gen_fsm).
-compile([{parse_transform, lager_transform}]).
-export([start_link/2]).
-export([sat_cmd_status/3]).
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
    usr_cmd
}).


%% =============================================================================
%%  Callbacks for ls1mcs_usr_cmd.
%% =============================================================================

%%
%%
%%
sat_cmd_status(_UsrCmdId, _SatCmdId, _Status) ->
    ok. % TODO



%% =============================================================================
%%  Callbacks for gen_fsm.
%% =============================================================================

%%
%%
%%
init({UsrCmd}) ->
    {ok, unknown, #state{usr_cmd = UsrCmd}}.


%%
%%
%%
handle_event(_Event, StateName, StateData = #state{}) ->
    {next_state, StateName, StateData}.


%%
%%  Other FSM callbacks.
%%
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


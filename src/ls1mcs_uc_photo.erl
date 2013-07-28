%%
%%  User command: photo.
%%  Takes photo, gets its metadata and downloads it.
%%
-module(ls1mcs_uc_photo).
-behaviour(gen_fsm).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0]).
-export([]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).



%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%
%%
start_link() ->
    gen_fsm:start_link({via, gproc, ?MODULE}, ?MODULE, {}, []). % TODO



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
}).


%% =============================================================================
%%  Callbacks for gen_fsm.
%% =============================================================================

%%
%%
%%
init({}) ->
    {ok, unknown, #state{}}.    % TODO


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


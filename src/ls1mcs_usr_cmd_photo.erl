%%
%%  User command: dlnk_photo.
%%  Downlinks photo metadata and downloads its content.
%%
-module(ls1mcs_usr_cmd_photo).
-behaviour(ls1mcs_usr_cmd).
-behaviour(gen_fsm).
-compile([{parse_transform, lager_transform}]).
-export([start_link/2]).
-export([sat_cmd_status/3]).
-export([starting/2, getting_meta/2, getting_data/2]).
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
    id,
    usr_cmd,
    last_cmd_id,    %% Last sat cmd ID.
    retry_meta,
    size            %% Photo size
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
init({UsrCmd = #usr_cmd{id = UsrCmdId}}) ->
    gen_fsm:send_event(self(), start),
    StateData = #state{
        id = UsrCmdId,
        usr_cmd = UsrCmd,
        retry_meta = 3
    },
    {ok, starting, StateData}.


%%
%%  FSM State: starting.
%%
starting(start, StateData) ->
    NewStateData = send_photo_meta(StateData),
    {next_state, getting_meta, NewStateData}.


%%
%%  FSM State: getting_meta.
%%
getting_meta({sat_cmd_status, SatCmdId, failed}, StateData = #state{last_cmd_id = SatCmdId, retry_meta = Retry}) ->
    if
        Retry > 0 ->
            NewStateData = send_photo_meta(StateData),
            {next_state, getting_meta, NewStateData};
        true ->
            lager:warning("ls1mcs_usr_cmd_photo: Unable to get metadata."),
            {stop, normal, StateData}
    end;

getting_meta({sat_cmd_status, SatCmdId, completed}, StateData = #state{last_cmd_id = SatCmdId}) ->
    CRef = ls1mcs_store:cref_from_sat_cmd_id(SatCmdId),
    {ok, [#ls1p_data_frame{data = Metadata}]} = ls1mcs_store:get_ls1p_frame({data, CRef}),
    {_PhotoCRef, _PhotoTime, PhotoSize} = ls1mcs_proto_ls1p:decode_photo_meta(Metadata),
    BlockSize = 195,
    BlockTill = (PhotoSize div BlockSize) + 2,
    NewStateData = send_photo_data(BlockSize, 0, BlockTill, StateData#state{size = PhotoSize}),
    {next_state, getting_data, NewStateData};

getting_meta({sat_cmd_status, SatCmdId, Status}, StateData) ->
    lager:warning(
        "ls1mcs_usr_cmd_photo: Ingoring unexpected sat_cmd_status, sat_cmd_id=~p, status=~p.",
        [SatCmdId, Status]
    ),
    {next_state, getting_meta, StateData}.


%%
%%  FSM State: getting_data.
%%
getting_data({sat_cmd_status, SatCmdId, failed}, StateData = #state{last_cmd_id = SatCmdId}) ->
    lager:warning("ls1mcs_usr_cmd_photo: Unable to get photo contents."),
    {stop, normal, StateData};

getting_data({sat_cmd_status, SatCmdId, completed}, StateData = #state{last_cmd_id = SatCmdId}) ->
    lager:warning("ls1mcs_usr_cmd_photo: Photo downloaded."),
    {stop, normal, StateData};

getting_data({sat_cmd_status, SatCmdId, Status}, StateData) ->
    lager:warning(
        "ls1mcs_usr_cmd_photo: Ingoring unexpected sat_cmd_status, sat_cmd_id=~p, status=~p.",
        [SatCmdId, Status]
    ),
    {next_state, getting_data, StateData}.


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
send_photo_meta(StateData = #state{id = UsrCmdId, retry_meta = Retry}) ->
    Frame = #ls1p_cmd_frame{
        addr = arduino,
        port = photo_meta,
        ack = false
    },
    {ok, SatCmdId} = ls1mcs_usr_cmd:send_sat_cmd(?MODULE, UsrCmdId, sat_cmd(Frame)),
    StateData#state{
        last_cmd_id = SatCmdId,
        retry_meta = Retry - 1
    }.


%%
%%
%%
send_photo_data(BlkSz, From, Till, StateData = #state{id = UsrCmdId}) ->
    Frame = #ls1p_cmd_frame{
        addr = arduino,
        port = photo_data,
        ack = false,
        data = <<BlkSz:8, From:16/little, Till:16/little>>
    },
    {ok, SatCmdId} = ls1mcs_usr_cmd:send_sat_cmd(?MODULE, UsrCmdId, sat_cmd(Frame)),
    StateData#state{
        last_cmd_id = SatCmdId
    }.


%%
%%
%%
sat_cmd(CmdFrame) ->
    #sat_cmd{
        cmd_frame = CmdFrame
    }.



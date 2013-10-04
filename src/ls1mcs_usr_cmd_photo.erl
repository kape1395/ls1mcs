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
    id,             %% User command id (the same as in usr_cmd, for convenience).
    usr_cmd,        %% User command.
    block_size,     %% Block size in bytes used to download the photo.
    last_cmd_id,    %% Last sat cmd ID.
    retry_meta,     %% Maximal number of times the photo_meta command can be sent.
    retry_data,     %% Maximal number of times the photo_data command can be sent.
    photo_size,     %% Size of the photo file in bytes.
    data_gaps       %% Intervals in bytes for whose the data is to be downloaded.
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
        block_size = 195,
        retry_meta = 3,
        retry_data = 10
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
    {PhotoSize} = ls1mcs_proto_ls1p:decode_photo_meta(Metadata),
    download(StateData#state{data_gaps = [{0, PhotoSize}], photo_size = PhotoSize});

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
    lager:warning("ls1mcs_usr_cmd_photo: Got photo_data failure."),
    download(StateData);

getting_data({sat_cmd_status, SatCmdId, completed}, StateData = #state{last_cmd_id = SatCmdId}) ->
    #state{
        id = UsrCmdId,
        photo_size = PhotoSize
    } = StateData,
    lager:warning("ls1mcs_usr_cmd_photo: Got photo_data response."),
    %
    %   Get collected data fragments.
    %
    PhotoDataPredicate = fun
        (#sat_cmd{cmd_frame = #ls1p_cmd_frame{addr = arduino, port = photo_data}}) -> true;
        (_) -> false
    end,
    Ls1pFramesFun = fun (#sat_cmd{id = SCId}) ->
        CRef = ls1mcs_store:cref_from_sat_cmd_id(SCId),
        {ok, CmdFrame} = ls1mcs_store:get_ls1p_frame({cmd, CRef}),
        {ok, DataFrames} = ls1mcs_store:get_ls1p_frame({data, CRef}),
        {CmdFrame, DataFrames}
    end,
    {ok, SatCmds} = ls1mcs_store:get_sat_cmds({usr_cmd, UsrCmdId}),
    PhotoDataSatCmds = lists:filter(PhotoDataPredicate, SatCmds),
    PhotoDataSatFrames = lists:map(Ls1pFramesFun, PhotoDataSatCmds),
    {ok, Fragments} = ls1mcs_proto_ls1p:merged_response_fragments(PhotoDataSatFrames),
    %
    %   Find gaps in the data.
    %
    FragmentsWithEnd = lists:sort([{PhotoSize, PhotoSize, <<>>} | Fragments]),
    GetGapsFun = fun ({From, Till, _Data}, {LastTill, Gaps}) ->
        case From > LastTill of
            true -> {Till, [{LastTill, From - LastTill} | Gaps]};
            false -> {Till, Gaps}
        end
    end,
    {PhotoSize, NewGaps} = lists:foldl(GetGapsFun, {0, []}, FragmentsWithEnd),
    %
    %   Download missing fragments (gaps).
    %
    download(StateData#state{data_gaps = NewGaps});

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
%%  Download first pending block of data.
%%
download(StateData = #state{data_gaps = []}) ->
    lager:info("ls1mcs_usr_cmd_photo: Photo downloaded."),
    {stop, normal, StateData};

download(StateData = #state{retry_data = Retry}) when Retry =< 0 ->
    lager:info("ls1mcs_usr_cmd_photo: Photo download failed (data retry count exceeded)."),
    {stop, normal, StateData};

download(StateData = #state{data_gaps = [FirstGap | _], retry_data = Retry, block_size = BlockSize}) ->
    {From, Length} = FirstGap,
    BlockFrom = in_block(From, BlockSize),
    BlockTill = in_block(From + Length, BlockSize) + 1,
    NewStateData = send_photo_data(BlockSize, BlockFrom, BlockTill, StateData#state{retry_data = Retry - 1}),
    {next_state, getting_data, NewStateData}.


%%
%%  Calculates block index for the specified byte.
%%
in_block(BytePos, BlockSize) ->
    BytePos div BlockSize.


%%
%%
%%
send_photo_meta(StateData = #state{id = UsrCmdId, retry_meta = Retry}) ->
    SatCmd = #sat_cmd{
        cmd_frame = #ls1p_cmd_frame{
            addr = arduino,
            port = photo_meta,
            ack = false
        },
        exp_dfc = 1
    },
    {ok, SatCmdId} = ls1mcs_usr_cmd:send_sat_cmd(?MODULE, UsrCmdId, SatCmd),
    StateData#state{
        last_cmd_id = SatCmdId,
        retry_meta = Retry - 1
    }.


%%
%%
%%
send_photo_data(BlkSz, From, Till, StateData = #state{id = UsrCmdId}) ->
    SatCmd = #sat_cmd{
        cmd_frame = #ls1p_cmd_frame{
            addr = arduino,
            port = photo_data,
            ack = false,
            data = <<BlkSz:8, From:16/little, Till:16/little>>
        },
        exp_dfc = Till - From
    },
    {ok, SatCmdId} = ls1mcs_usr_cmd:send_sat_cmd(?MODULE, UsrCmdId, SatCmd),
    StateData#state{
        last_cmd_id = SatCmdId
    }.



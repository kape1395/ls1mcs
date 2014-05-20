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
%%  User command: dlnk_photo.
%%
%%  Downlinks photo metadata and downloads its content. This command can
%%  also work without knowing the photo metadata (photo size).
%%
%%  The command switches to the manual mode, if manual download request
%%  has been made or when download retries were exceeded.
%%
-module(ls1mcs_usr_cmd_photo).
-behaviour(ls1mcs_usr_cmd).
-behaviour(gen_fsm).
-compile([{parse_transform, lager_transform}]).
-export([start_link/2, get_running/0, close/1, get_missing/1, get_photo/1, download/3]).
-export([sat_cmd_status/3]).
-export([starting/2, getting_meta/2, getting_data/2, completed/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-include("ls1mcs.hrl").

-define(REF(UsrCmdId), {via, gproc, {n, l, {?MODULE, UsrCmdId}}}).



%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%  Start this command.
%%
-spec start_link(#usr_cmd{}, #usr_cmd_spec{})
        -> {ok, pid()} | term().

start_link(UsrCmd = #usr_cmd{id = UsrCmdId}, _UsrCmdSpec) ->
    gen_fsm:start_link(?REF(UsrCmdId), ?MODULE, {UsrCmd}, []).


%%
%%  Returns list of currently running commands (Command ids).
%%
get_running() ->
    UsrCmdIds = [ UCId || {_Pid, UCId} <- gproc:lookup_values({p, l, ?MODULE})],
    {ok, UsrCmdIds}.


%%
%%  Stop execution of this command.
%%
close(UsrCmdId) ->
    gen_fsm:send_event(?REF(UsrCmdId), close).


%%
%%  Returns missing block ranges.
%%
get_missing(UsrCmdId) ->
    gen_fsm:sync_send_all_state_event(?REF(UsrCmdId), get_missing).


%%
%%  Returns photo content.
%%
get_photo(UsrCmdId) ->
    case ls1mcs_store:get_usr_cmd(UsrCmdId, all) of
        {ok, #usr_cmd{spec = Spec}, SatCmds} when Spec =:= photo_data; Spec =:= dlnk_photo ->
            Frames = [ {CmdFrame, DataFrames} ||
                {
                    _SatCmd,
                    CmdFrame = #ls1p_cmd_frame{addr = arduino, port = photo_data},
                    _AckFrame,
                    DataFrames
                } <- SatCmds
            ],
            {ok, _PhotoContent} = ls1mcs_proto_ls1p:merged_response(Frames);
        {error, Reason} ->
            {error, Reason}
    end.


%%
%%  Peform manual download of the photo data.
%%  This action transfers the command to the manual mode.
%%
download(UsrCmdId, BlockFrom, BlockTill) ->
    lager:debug("Got request to download blocks [~p, ~p) for usr_cmd_id=~p", [BlockFrom, BlockTill, UsrCmdId]),
    gen_fsm:send_event(?REF(UsrCmdId), {download, BlockFrom, BlockTill}).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    id,             %% User command id (the same as in usr_cmd, for convenience).
    usr_cmd,        %% User command.
    mode,
    block_size,     %% Block size in bytes used to download the photo.
    last_cmd_id,    %% Last sat cmd ID.
    retry_meta,     %% Maximal number of times the photo_meta command can be sent (used if mode=auto).
    retry_data,     %% Maximal number of times the photo_data command can be sent (used if mode=auto).
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
init({UsrCmd = #usr_cmd{id = UsrCmdId, args = Args}}) ->
    true = gproc:reg({p, l, ?MODULE}, UsrCmdId),
    Manual    = ls1mcs_usr_cmd:arg_value(manual,     Args, boolean, false),
    BlockSize = ls1mcs_usr_cmd:arg_value(block_size, Args, integer, 210),
    RetryMeta = ls1mcs_usr_cmd:arg_value(retry_meta, Args, integer, 5),
    RetryData = ls1mcs_usr_cmd:arg_value(retry_data, Args, integer, 1),

    gen_fsm:send_event(self(), start),
    StateData = #state{
        id = UsrCmdId,
        usr_cmd = UsrCmd,
        mode = case Manual of true -> manual; false -> auto end,
        block_size = BlockSize,
        retry_meta = RetryMeta,
        retry_data = RetryData
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
            lager:warning("Unable to get metadata."),
            case download(StateData#state{data_gaps = [{0, undefined}], photo_size = undefined}) of
                {cont, NewStateData} -> {next_state, getting_data, NewStateData};
                {stop, NewStateData} -> {next_state, getting_data, NewStateData#state{mode = manual}};
                {done, NewStateData} -> {next_state, completed,    NewStateData}
            end
    end;

getting_meta({sat_cmd_status, SatCmdId, completed}, StateData = #state{last_cmd_id = SatCmdId}) ->
    CRef = ls1mcs_store:cref_from_sat_cmd_id(SatCmdId),
    {ok, [#ls1p_data_frame{data = Metadata}]} = ls1mcs_store:get_ls1p_frame({data, CRef}),
    {ok, PhotoSize} = ls1mcs_proto_ls1p:decode_photo_meta(Metadata),
    case download(StateData#state{data_gaps = [{0, PhotoSize}], photo_size = PhotoSize}) of
        {cont, NewStateData} -> {next_state, getting_data, NewStateData};
        {stop, NewStateData} -> {next_state, getting_data, NewStateData#state{mode = manual}};
        {done, NewStateData} -> {next_state, completed,    NewStateData}
    end;

getting_meta({sat_cmd_status, SatCmdId, Status}, StateData) ->
    lager:warning(
        "Ingoring unexpected sat_cmd_status, sat_cmd_id=~p, status=~p.",
        [SatCmdId, Status]
    ),
    {next_state, getting_meta, StateData};

getting_meta({download, BlockFrom, BlockTill}, StateData = #state{block_size = BlockSize}) ->
    NewStateData = send_photo_data(BlockSize, BlockFrom, BlockTill, StateData),
    {next_state, getting_data, NewStateData#state{mode = manual}};

getting_meta(close, StateData) ->
    {stop, normal, StateData}.


%%
%%  FSM State: getting_data.
%%
getting_data({sat_cmd_status, SatCmdId, failed}, StateData = #state{last_cmd_id = SatCmdId}) ->
    lager:warning("Got photo_data failure."),
    case download(StateData) of
        {cont, NewStateData} -> {next_state, getting_data, NewStateData};
        {stop, NewStateData} -> {next_state, getting_data, NewStateData#state{mode = manual}};
        {done, NewStateData} -> {next_state, completed,    NewStateData}
    end;

getting_data({sat_cmd_status, SatCmdId, completed}, StateData = #state{last_cmd_id = SatCmdId}) ->
    lager:debug("Got photo_data response."),
    {ok, UpdatedGaps} = collect_data_gaps(StateData),
    case download(StateData#state{data_gaps = lists:sort(UpdatedGaps)}) of
        {cont, NewStateData} -> {next_state, getting_data, NewStateData};
        {stop, NewStateData} -> {next_state, getting_data, NewStateData#state{mode = manual}};
        {done, NewStateData} -> {next_state, completed,    NewStateData}
    end;

getting_data({sat_cmd_status, SatCmdId, Status}, StateData) ->
    lager:warning(
        "Ingoring unexpected sat_cmd_status, sat_cmd_id=~p, status=~p.",
        [SatCmdId, Status]
    ),
    {next_state, getting_data, StateData};

getting_data({download, BlockFrom, BlockTill}, StateData = #state{block_size = BlockSize}) ->
    NewStateData = send_photo_data(BlockSize, BlockFrom, BlockTill, StateData),
    {next_state, getting_data, NewStateData#state{mode = manual}};

getting_data(close, StateData) ->
    {stop, normal, StateData}.


%%
%%  FSM State: `completed`.
%%
completed(close, StateData) ->
    {stop, normal, StateData};

completed(Event, StateData) ->
    lager:debug("Ignoring event in the completed state: ~p", [Event]),
    {next_state, completed, StateData}.


%%
%%  Other FSM callbacks.
%%
handle_event(_Event, StateName, StateData = #state{}) ->
    {next_state, StateName, StateData}.

handle_sync_event(get_missing, From, StateName, StateData = #state{block_size = BlockSize}) ->
    {ok, UpdatedGaps} = collect_data_gaps(StateData),
    GapsToBlocks = fun (Gap) ->
        {ok, BlockFrom, BlockTill} = gap_to_blocks(Gap, BlockSize),
        {BlockFrom, BlockTill}
    end,
    MissingBlocks = lists:map(GapsToBlocks, UpdatedGaps),
    gen_fsm:reply(From, {ok, MissingBlocks}),
    {next_state, StateName, StateData#state{data_gaps = UpdatedGaps}}.

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
    lager:info("Photo downloaded."),
    {done, StateData};

download(StateData = #state{mode = auto, retry_data = Retry}) when Retry =< 0 ->
    lager:info("Photo download failed (data retry count exceeded)."),
    {stop, StateData};

download(StateData = #state{mode = auto, data_gaps = AllGaps = [FirstGap | _], retry_data = Retry, block_size = BlockSize}) ->
    {ok, BlockFrom, BlockTill} = gap_to_blocks(FirstGap, BlockSize),
    AdjustedBlockFrom = max(BlockFrom - 3, 0),
    AdjustedBlockTill = case BlockTill of
        undefined -> BlockFrom + 100;
        BlockTill -> BlockTill + 3
    end,
    lager:debug("Have gaps in data {From, Length}: ~p", [AllGaps]),
    lager:info(
        "Downloading data for gap: block_size=~p, block_from=~p, block_till=~p",
        [BlockSize, BlockFrom, BlockTill]
    ),
    NewStateData = send_photo_data(
        BlockSize,
        AdjustedBlockFrom,
        AdjustedBlockTill,
        StateData#state{retry_data = Retry - 1}
    ),
    {cont, NewStateData};

download(StateData = #state{mode = manual}) ->
    {cont, StateData}.


%%
%%  Calculates block index for the specified byte.
%%
in_block(BytePos, BlockSize) ->
    BytePos div BlockSize.


%%
%%  Convert gap in bytes to block range.
%%
gap_to_blocks({From, undefined}, BlockSize) ->
    BlockFrom = in_block(From, BlockSize),
    {ok, BlockFrom, undefined};

gap_to_blocks({From, Length}, BlockSize) ->
    BlockFrom = in_block(From, BlockSize),
    BlockTill = in_block(From + Length, BlockSize) + 1,
    {ok, BlockFrom, BlockTill}.


%%
%%  Collects data gaps.
%%
collect_data_gaps(StateData) ->
    #state{
        id = UsrCmdId,
        photo_size = PhotoSize
    } = StateData,
    lager:debug("Got photo_data response."),
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
    Ls1pWithEOFFun = fun
        (#ls1p_data_frame{eof = true},  _)    -> true;
        (#ls1p_data_frame{eof = false}, Prev) -> Prev
    end,
    {ok, SatCmds} = ls1mcs_store:get_sat_cmds({usr_cmd, UsrCmdId}),
    PhotoDataSatCmds = lists:filter(PhotoDataPredicate, SatCmds),
    PhotoDataSatFrames = lists:map(Ls1pFramesFun, PhotoDataSatCmds),
    PhotoDataHaveEOF = lists:foldl(Ls1pWithEOFFun, false, lists:append([ DFs || {_CF, DFs} <- PhotoDataSatFrames ])),
    {ok, Fragments} = ls1mcs_proto_ls1p:merged_response_fragments(PhotoDataSatFrames),
    %
    %   Find gaps in the data.
    %
    GetGapsFun = fun ({From, Till, _Data}, {LastTill, Gaps}) ->
        case From > LastTill of
            true -> {Till, [{LastTill, From - LastTill} | Gaps]};
            false -> {Till, Gaps}
        end
    end,
    UpdatedGaps = case PhotoSize of
        undefined ->
            {LastFragEnd, NewGaps} = lists:foldl(GetGapsFun, {0, []}, Fragments),
            case PhotoDataHaveEOF of
                true  -> [{LastFragEnd, undefined} | NewGaps]; % NOTE: was `NewGaps;`; adding last interval, because we can have EOF for eash dlnk command.
                false -> [{LastFragEnd, undefined} | NewGaps]
            end;
        PhotoSize when is_integer(PhotoSize) ->
            FragmentsWithEnd = lists:sort([{PhotoSize, PhotoSize, <<>>} | Fragments]),
            {PhotoSize, NewGaps} = lists:foldl(GetGapsFun, {0, []}, FragmentsWithEnd),
            NewGaps
    end,
    {ok, UpdatedGaps}.


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



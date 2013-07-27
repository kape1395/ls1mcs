-module(ls1mcs_proto_ls1p).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-behaviour(ls1mcs_protocol).
-export([start_link/3, decode_tm/1, merged_response/1, merged_response/2]).
-export([send/2, received/2]).
-export([encode/1, decode/1]). % For tests.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("ls1mcs.hrl").
-include("ls1p.hrl").

-define(GROUND_ADDR, 2#111).
-define(GROUND_PORT_ACK,  16#0).
-define(GROUND_PORT_DATA, 16#1).
-define(GROUND_PORT_TM,   16#2).


-define(ADDR_MAP, [
    {arm,       0},
    {arduino,   1},
    {eps,       2},
    {gps,       3},
    {helium,    4},
    {ground,    ?GROUND_ADDR}
]).
-define(PORT_MAP, [
    {arm,       ping,       16#0},
    {arm,       kill,       16#1},
    {arm,       downlink,   16#2},
    {arm,       runtime_tm, 16#3},
    {arm,       job_period, 16#4},
    {arm,       multi,      16#F},
    {arduino,   take_photo, 16#0},
    {arduino,   photo_meta, 16#1},
    {arduino,   photo_data, 16#2},
    {eps,       command,    16#0},
    {gps,       nmea,       16#0},
    {gps,       binary,     16#1},
    {helium,    command,    16#0},
    {ground,    ack,        ?GROUND_PORT_ACK},
    {ground,    data,       ?GROUND_PORT_DATA},
    {ground,    telemetry,  ?GROUND_PORT_TM}
]).


%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%
%%
start_link(Name, Lower, Upper) ->
    gen_server:start_link({via, gproc, Name}, ?MODULE, {Lower, Upper}, []).


%%
%%
%%
send(Ref, Data) when is_record(Data, ls1p_cmd_frame) ->
    gen_server:cast({via, gproc, Ref}, {send, Data}).


%%
%%  Not used here.
%%
received(Ref, Data) when is_binary(Data) ->
    gen_server:cast({via, gproc, Ref}, {received, Data}).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    lower,      %% Lower protocol ref.
    upper       %% Upper protocol ref.
}).


%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================


%%
%%
%%
init({Lower, Upper}) ->
    self() ! {initialize},
    {ok, #state{lower = Lower, upper = Upper}}.


%%
%%
%%
handle_call(_Message, _From, State) ->
    {stop, not_implemented, State}.


%%
%%
%%
handle_cast({send, Frame}, State = #state{lower = Lower}) ->
    {ok, DataBin} = encode(Frame),
    ok = ls1mcs_store:add_ls1p_frame(Frame, DataBin, erlang:now()),
    ok = ls1mcs_protocol:send(Lower, DataBin),
    {noreply, State};

handle_cast({received, DataBin}, State = #state{upper = Upper}) ->
    try decode(DataBin) of
        {ok, Frame} when is_record(Frame, ls1p_cmd_frame) ->
            lager:debug("ls1mcs_proto_ls1p: Dropping received (echoed?) command frame: ~p", [Frame]);
        {ok, Frame} ->
            lager:debug("ls1mcs_proto_ls1p: Decoded frame: ~p from ~p", [Frame, DataBin]),
            ok = ls1mcs_store:add_ls1p_frame(Frame, DataBin, erlang:now()),
            ok = ls1mcs_protocol:received(Upper, Frame)
    catch
        ErrType:ErrCode ->
            lager:debug(
                "ls1mcs_proto_ls1p: Received invalid frame: ~p, error=~p:~p, trace=~p",
                [DataBin, ErrType, ErrCode, erlang:get_stacktrace()]
            ),
            ok = ls1mcs_store:add_unknown_frame(DataBin, erlang:now())
    end,
    {noreply, State}.


%%
%%  Deffered initialization.
%%
handle_info({initialize}, State = #state{lower = Lower, upper = Upper}) ->
    ls1mcs_protocol:await(Lower),
    ls1mcs_protocol:await(Upper),
    {noreply, State}.


%%
%%
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

%%
%%  LS1P encoder.
%%

%%  Acknowledgement frame.
encode(Frame) when is_record(Frame, ls1p_ack_frame) ->
    #ls1p_ack_frame{
        status = Status,
        cref = {_Epoch, CRef},
        recv_status = RecvStatus
    } = Frame,
    StatusBin = encode_bool(Status),
    FrameBin = <<?GROUND_ADDR:3, ?GROUND_PORT_ACK:4, StatusBin:1, CRef:16, RecvStatus:8>>,
    {ok, FrameBin};

%%  Data frame.
encode(Frame) when is_record(Frame, ls1p_data_frame) ->
    #ls1p_data_frame{
        eof = Eof,
        cref = {_Epoch, CRef},
        fragment = Fragment,
        data = Data
    } = Frame,
    EofBin = encode_bool(Eof),
    FrameBin = <<?GROUND_ADDR:3, ?GROUND_PORT_DATA:4, EofBin:1, CRef:16, Fragment:16, Data/binary>>,
    {ok, FrameBin};

%%  Telemetry frame.
encode(Frame) when is_record(Frame, ls1p_tm_frame) ->
    #ls1p_tm_frame{
        data = Telemetry
    } = Frame,
    FrameBin = <<?GROUND_ADDR:3, ?GROUND_PORT_TM:4, 0:1, Telemetry/binary>>,
    {ok, FrameBin};

%%  Command frame.
encode(Frame) when is_record(Frame, ls1p_cmd_frame) ->
    #ls1p_cmd_frame{
        addr = Addr, port = Port, ack = Ack,
        cref = {_Epoch, CRef}, delay = Delay, data = Data
    } = Frame,
    AddrBin = encode_addr(Addr),
    PortBin = encode_port(Addr, Port),
    AckBin  = encode_bool(Ack),
    FrameBin = <<AddrBin:3, PortBin:4, AckBin:1, CRef:16, Delay:16, Data/binary>>,
    {ok, FrameBin}.




%%
%%  LS1P decoder.
%%

%%  Acknowledgement frame.
decode(<<?GROUND_ADDR:3, ?GROUND_PORT_ACK:4, StatusBin:1, CRef:16, RecvStatus:8>>) ->
    Status = decode_bool(StatusBin),
    Frame = #ls1p_ack_frame{
        status = Status,
        cref = {undefined, CRef},
        recv_status = RecvStatus
    },
    {ok, Frame};

%%  Data frame
decode(<<?GROUND_ADDR:3, ?GROUND_PORT_DATA:4, EofBin:1, CRef:16, Fragment:16, Data/binary>>) ->
    Eof = decode_bool(EofBin),
    Frame = #ls1p_data_frame{
        eof = Eof,
        cref = {undefined, CRef},
        fragment = Fragment,
        data = Data
    },
    {ok, Frame};

%%  Telemetry frame
decode(<<?GROUND_ADDR:3, ?GROUND_PORT_TM:4, 0:1, Telemetry/binary>>) ->
    Frame = #ls1p_tm_frame{
        data = Telemetry
    },
    {ok, Frame};

%%  Command frame
decode(<<AddrBin:3, PortBin:4, AckBin:1, CRef:16, Delay:16, Data/binary>>) ->
    Addr = decode_addr(AddrBin),
    Port = decode_port(Addr, PortBin),
    Ack = decode_bool(AckBin),
    Frame = #ls1p_cmd_frame{
        addr = Addr,
        port = Port,
        ack = Ack,
        cref = {undefined, CRef},
        delay = Delay,
        data = Data
    },
    {ok, Frame}.


%%
%%  Address encoding/decoding.
%%
encode_addr(Addr) ->
    {Addr, AddrBin} = lists:keyfind(Addr, 1, ?ADDR_MAP),
    AddrBin.


decode_addr(AddrBin) ->
    {Addr, AddrBin} = lists:keyfind(AddrBin, 2, ?ADDR_MAP),
    Addr.


%%
%%  Port encoding/decoding.
%%
encode_port(Addr, Port) ->
    FilterFun = fun ({A, P, _}) -> (A =:= Addr andalso P =:= Port) end,
    [{Addr, Port, PortBin}] = lists:filter(FilterFun, ?PORT_MAP),
    PortBin.

decode_port(Addr, PortBin) ->
    FilterFun = fun ({A, _, P}) -> (A =:= Addr andalso P =:= PortBin) end,
    [{Addr, Port, PortBin}] = lists:filter(FilterFun, ?PORT_MAP),
    Port.


%%
%%  Boolean encoding/decoding.
%%
encode_bool(false) -> 0;
encode_bool(true) -> 1.

decode_bool(0) -> false;
decode_bool(1) -> true.


%% =============================================================================
%%  Telemetry decoder.
%% =============================================================================

%%
%%  Decode entire telemetry frame (without frame header byte).
%%  Total length: 233 bytes
%%
decode_tm(Telemetry) when is_binary(Telemetry) ->
    <<
        Time:32,
        EPS:43/binary,
        He:16/binary,
        Att1:34/binary,
        Att2:34/binary,
        Att3:34/binary,
        Att4:34/binary,
        Att5:34/binary
    >> = Telemetry,
    #tm{
        time = Time,
        eps = decode_tm_eps(EPS),
        he = decode_tm_he(He),
        att = [
            decode_tm_att(Att1),
            decode_tm_att(Att2),
            decode_tm_att(Att3),
            decode_tm_att(Att4),
            decode_tm_att(Att5)
        ]
    }.

%%
%%  Decode attitude data.
%%
decode_tm_att(Telemetry) ->
    <<
        Mag:6/binary,
        MPU:14/binary,
        Gyro1:7/binary,
        Gyro2:7/binary
    >> = Telemetry,
    #tm_att{
        mag = decode_tm_mag(Mag),
        mpu = decode_tm_mpu(MPU),
        gyro_1 = decode_tm_gyro(Gyro1),
        gyro_2 = decode_tm_gyro(Gyro2)
    }.


decode_tm_eps(Telemetry) ->
    OnOffFun = fun
        (1) -> on;
        (0) -> off
    end,
    PPTModeFun = fun
        (0) -> 'Hardware';
        (1) -> 'MPPT';
        (2) -> 'Fixed_SW_PPT'
    end,
    <<
        PV1:16,
        PV2:16,
        PV3:16,
        PC:16,
        BV:16,
        SC:16,
        TempBC1:16/signed,
        TempBC2:16/signed,
        TempBC3:16/signed,
        TempOB:16/signed,
        BattTemp1:16/signed,
        BattTemp2:16/signed,
        Latchup50V1:16,
        Latchup50V2:16,
        Latchup50V3:16,
        Latchup33V1:16,
        Latchup33V2:16,
        Latchup33V3:16,
        Reset:8,
        Bootcount:16,
        SwErrors:16,
        PPTMode:8,
        ChannelStatusQH:1,
        ChannelStatusQS:1,
        ChannelStatus33V3:1,
        ChannelStatus33V2:1,
        ChannelStatus33V1:1,
        ChannelStatus50V3:1,
        ChannelStatus50V2:1,
        ChannelStatus50V1:1
    >> = Telemetry,
    #tm_eps{
        pv_1 = PV1,
        pv_2 = PV2,
        pv_3 = PV3,
        pc = PC,
        bv = BV,
        sc = SC,
        temp_BC1 = TempBC1,
        temp_BC2 = TempBC2,
        temp_BC3 = TempBC3,
        temp_OB = TempOB,
        batt_temp_1 = BattTemp1,
        batt_temp_2 = BattTemp2,
        latchup_50V1 = Latchup50V1,
        latchup_50V2 = Latchup50V2,
        latchup_50V3 = Latchup50V3,
        latchup_33V1 = Latchup33V1,
        latchup_33V2 = Latchup33V2,
        latchup_33V3 = Latchup33V3,
        reset           = Reset,
        bootcount       = Bootcount,
        sw_errors       = SwErrors,
        ppt_mode        = PPTModeFun(PPTMode),
        channel_status_QH   = OnOffFun(ChannelStatusQH),
        channel_status_QS   = OnOffFun(ChannelStatusQS),
        channel_status_50V1 = OnOffFun(ChannelStatus50V1),
        channel_status_50V2 = OnOffFun(ChannelStatus50V2),
        channel_status_50V3 = OnOffFun(ChannelStatus50V3),
        channel_status_33V1 = OnOffFun(ChannelStatus33V1),
        channel_status_33V2 = OnOffFun(ChannelStatus33V2),
        channel_status_33V3 = OnOffFun(ChannelStatus33V3)
    }.

%%
%%  Decode He-100 TM.
%%
decode_tm_he(Telemetry) ->
    <<
        OpCounter:16,
        MSP430Temp:16/signed,
        TimeCount1:8,
        TimeCount2:8,
        TimeCount3:8,
        RSSI:8,
        BytesReceived:32,
        BytesTransmitted:32
    >> = Telemetry,
    #tm_he{
        op_counter = OpCounter,
        msp430_temp = MSP430Temp,
        time_count_1 = TimeCount1,
        time_count_2 = TimeCount2,
        time_count_3 = TimeCount3,
        rssi = RSSI,
        bytes_received = BytesReceived,
        bytes_transmitted = BytesTransmitted
    }.

%%
%%  Decode HMC5883L TM.
%%
decode_tm_mag(Telemetry) ->
    <<
        Bx:16,
        By:16,
        Bz:16
    >> = Telemetry,
    #tm_mag{
        bx = Bx,
        by = By,
        bz = Bz
    }.


%%
%%  Decode MPU-6000A TM.
%%
decode_tm_mpu(Telemetry) ->
    <<
        Gx:16,
        Gy:16,
        Gz:16,
        Ax:16,
        Ay:16,
        Az:16,
        Temp:16
    >> = Telemetry,
    #tm_mpu{
        gx = Gx,
        gy = Gy,
        gz = Gz,
        ax = Ax,
        ay = Ay,
        az = Az,
        temp = Temp
    }.


%%
%%  Decode L3GD20 TM.
%%
decode_tm_gyro(Telemetry) ->
    <<
        Wx:16,
        Wy:16,
        Wz:16,
        Temp:8
    >> = Telemetry,
    #tm_gyro{
        wx = Wx,
        wy = Wy,
        wz = Wz,
        temp = Temp
    }.



%% =============================================================================
%%  Data reconstruction.
%% =============================================================================

%%
%%  Merge photo.
%%
-spec merged_response(#ls1p_cmd_frame{}, [#ls1p_data_frame{}])
        -> {ok, binary()}.

merged_response(CmdFrame, DataFrames) ->
    merged_response([{CmdFrame, DataFrames}]).



%%
%%  Merge photo, downloaded by multiple downlink commands.
%%

-spec merged_response([{#ls1p_cmd_frame{}, [#ls1p_data_frame{}]}])
        -> {ok, binary()}.

merged_response(Commands) ->
    BlockSets = [ merge_to_blocks(C, D) || {C, D} <- Commands ],
    MergedBlocks = lists:foldl(fun overlay_block_sets/2, [], BlockSets),
    FillGaps = fun ({From, Till, Data}, {LastTill, Blocks}) ->
        ZerosBitLen = (From - LastTill) * 8,
        {Till, [Data, <<0:ZerosBitLen>> | Blocks]}
    end,
    {_, MergedIOList} = lists:foldl(FillGaps, {0, []}, MergedBlocks),
    Flattened = erlang:iolist_to_binary(lists:reverse(MergedIOList)),
    {ok, Flattened}.

%%
%%  Applies set of overlay blocks to set of base blocks.
%%  Input must be sorted?
%%
overlay_block_sets(Overlay, []) ->
    Overlay;

overlay_block_sets(Overlay, Base) ->
    lists:foldl(fun overlay_block/2, Base, Overlay).


%%
%%  Applies single overlay block to a set of base blocks.
%%
overlay_block(OverlayBlock, BaseBlocks) ->
    F = fun(B, {Before, O, After}) ->
        case overlay(O, B) of
            overlay_before_base -> {Before, O, [B | After]};
            overlay_after_base -> {[B | Before], O, After};
            NewOverlayedBlock ->  {Before, NewOverlayedBlock, After}
        end
    end,
    {BeforeOverlay, OverlayedBlock, AfterOverlay} = lists:foldl(F, {[], OverlayBlock, []}, BaseBlocks),
    lists:reverse(BeforeOverlay) ++ [OverlayedBlock] ++ lists:reverse(AfterOverlay).


%%
%%  Overlays two blocks. The first one has priority over the second one.
%%
overlay({_OFrom, OTill, _OData}, {BFrom, _BTill, _BData}) when OTill < BFrom -> overlay_before_base;
overlay({OFrom, _OTill, _OData}, {_BFrom, BTill, _BData}) when OFrom > BTill -> overlay_after_base;

% Overlay block covers entire base block.
overlay({OFrom, OTill, OData}, {BFrom, BTill, _BData}) when OFrom =< BFrom, OTill >= BTill ->
    {OFrom, OTill, OData};

% Overlay block is in the middle of the base block.
overlay({OFrom, OTill, OData}, {BFrom, BTill, BData}) when OFrom > BFrom, OTill < BTill ->
    PrefixSize = BFrom - OFrom,
    OverlappedSize = OTill - OFrom,
    <<Prefix:PrefixSize/binary, _:OverlappedSize/binary, Suffix/binary>> = BData,
    {BFrom, BTill, <<Prefix/binary, OData/binary, Suffix/binary>>};

% Overlay block covers front of the base block (or touches it).
overlay({OFrom, OTill, OData}, {BFrom, BTill, BData}) when OFrom =< BFrom ->
    OverlappedSize = BFrom - OTill,
    <<_:OverlappedSize/binary, Suffix>> = BData,
    {OFrom, BTill, <<OData/binary, Suffix/binary>>};

% Overlay block covers tail of the base block (or touches it).
overlay({OFrom, OTill, OData}, {BFrom, BTill, BData}) when OTill >= BTill ->
    PrefixSize = OFrom - BFrom,
    <<Prefix:PrefixSize/binary, _Suffix/binary>> = BData,
    {BFrom, OTill, <<Prefix/binary, OData/binary>>}.


%%
%%  Create non-overlapping and non-touching blocks out of data frames,
%%  that are response to a single command frame.
%%
%%  Reverse with usort is needed here to drop old duplicated data.
%%  TODO: Sort by time desc explicitly.
%%
merge_to_blocks(_CmdFrame, []) ->
    [];

merge_to_blocks(CmdFrame, DataFrames) ->
    {ok, BlockSize, FromBlock, _TillBlock} = fragment_metainfo(CmdFrame),
    SortedDataFrames = lists:ukeysort(#ls1p_data_frame.fragment, lists:reverse(DataFrames)),
    %
    FrameToBlock = fun (#ls1p_data_frame{fragment = F, data = D}) ->
        From = BlockSize * (FromBlock + F),
        {From, From + byte_size(D), D}
    end,
    %
    MergeFrames = fun (Frame, [LastBlock = {LastFrom, LastTill, LastData} | OtherBlocks]) ->
        NewBlock = {From, Till, Data} = FrameToBlock(Frame),
        case From =:= LastTill of
            true ->
                MergedBlock = {LastFrom, Till, <<LastData/binary, Data/binary>>},
                [MergedBlock | OtherBlocks];
            false ->
                [NewBlock, LastBlock | OtherBlocks]
        end
    end,
    %
    [FirstDF | OtherDFs] = SortedDataFrames,
    lists:reverse(lists:foldl(MergeFrames, [FrameToBlock(FirstDF)], OtherDFs)).


%%
%%  Extracts meta-info from the specified frame.
%%
fragment_metainfo(#ls1p_cmd_frame{addr = arduino, port = photo_data, data = Data}) ->
    <<BlockSize:8, From:16, Till:16>> = Data,
    {ok, BlockSize, From, Till}.

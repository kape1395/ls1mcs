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
%%  Implementation of the LS1P protocol.
%%
-module(ls1mcs_proto_ls1p).
-behaviour(ls1mcs_proto).
-compile([{parse_transform, lager_transform}]).
-export([make_ref/2, preview/1]).
-export([init/1, send/2, recv/2]).
-export([
    decode_tm/1, decode_photo_meta/1,
    merged_response_fragments/1, merged_archive_fragments/1,
    merged_response/1, merged_response/2
]).
-include("ls1mcs.hrl").
-include("ls1p.hrl").


-define(ADDR_MAP, [
    {arm,       0},
    {arduino,   1},
    {eps,       2},
    {gps,       3},
    {helium,    4},
    {ground,    ?GROUND_ADDR}
]).
-define(PORT_MAP, [
    {arm,   ping,           16#0},
    {arm,   kill,           16#1},
    {arm,   downlink,       16#2},
    {arm,   runtime_tm,     16#3},
    {arm,   job_period,     16#4},
    {arm,   pwr_allow_nm,   16#5},
    {arm,   pwr_state,      16#6},
    {arm,   term_sci_mode,  16#7},
    {arm,   start_fmrep,    16#8},
    {arm,   sd_format,      16#9},
    {arm,   set_started,    16#A},
    {arm,   multi,          16#F},
    {arduino,   take_photo, 16#0},
    {arduino,   photo_meta, 16#1},
    {arduino,   photo_data, 16#2},
    {arduino,   beacon_st,  16#3},
    {eps,       ch_status,  16#0},
    {eps,       hrd_reset,  16#1},
    {gps,       nmea,       16#0},
    {gps,       binary,     16#1},
    {helium,    restore,    16#0},
    {helium,    tx_pwr,     16#1},
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
make_ref(Password, LogFrames) ->
    ls1mcs_proto:make_ref(?MODULE, {Password, LogFrames}).


%%
%%
%%
preview(Frame) when is_binary(Frame) ->
    {ok, [Decoded]} = preview([Frame]),
    {ok, Decoded};

preview(Frames) when is_list(Frames) ->
    DecodeFun = fun
        (undefined) ->
            undefined;
        (Frame) ->
            case catch decode(Frame) of
                {ok, Decoded} ->
                    Decoded;
                _Error ->
                    undefined
            end
    end,
    {ok, lists:map(DecodeFun, Frames)}.



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    pass,
    log
}).



%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================

%%
%%
%%
init({Password, LogFrames}) ->
    {ok, #state{pass = Password, log = LogFrames}}.


%%
%%
%%
send(Frame, State = #state{pass = Password, log = LogFrames}) when is_record(Frame, ls1p_cmd_frame) ->
    {ok, DataBin} = encode(Frame, Password),
    {ok, _FrameWithCRef} = log_ls1p_frame(Frame, DataBin, LogFrames),
    {ok, [DataBin], State}.


%%
%%
%%
recv(DataBin, State = #state{pass = Password, log = LogFrames}) when is_binary(DataBin) ->
    case check_signature(DataBin, Password) of
        {ok, Frame} when Password =/= undefined ->
            % Handles echoed command in the case when password is specified.
            lager:debug("ls1mcs_proto_ls1p: Dropping received (echoed?) signed command frame: ~p", [Frame]),
            {ok, [], State};
        {error, _Reason} ->
            try decode(DataBin) of
                {ok, Frame} when is_record(Frame, ls1p_cmd_frame) ->
                    % Handles echoed command in the case when password is not specified.
                    lager:debug("ls1mcs_proto_ls1p: Dropping received (echoed?) command frame: ~p", [Frame]),
                    {ok, [], State};
                {ok, Frame} ->
                    lager:debug("ls1mcs_proto_ls1p: Decoded frame: ~p from ~p", [Frame, DataBin]),
                    {ok, FrameWithCRef} = log_ls1p_frame(Frame, DataBin, LogFrames),
                    {ok, [FrameWithCRef], State}
            catch
                ErrType:ErrCode ->
                    lager:debug(
                        "ls1mcs_proto_ls1p: Received invalid frame: ~p, error=~p:~p, trace=~p",
                        [DataBin, ErrType, ErrCode, erlang:get_stacktrace()]
                    ),
                    ok = log_unkn_frame(DataBin, LogFrames),
                    {ok, [], State}
            end
    end.



%% =============================================================================
%%  Internal Functions.
%% =============================================================================

%%
%%  Logs LS1P frames if needed.
%%  NOTE: The logging is implemented in this module, because we have
%%  here both the frame and its binary representation in all cases.
%%
log_ls1p_frame(Frame, Binary, true) ->
    {ok, _FrameWithCRef} = ls1mcs_store:add_ls1p_frame(Frame, Binary, erlang:now());

log_ls1p_frame(Frame, _Binary, false) ->
    {ok, Frame}.


%%
%%  Logs unknown frames if needed.
%%  NOTE: The logging is implemented in this module, because we have
%%  here both the frame and its binary representation in all cases.
%%
log_unkn_frame(Binary, true) ->
    ok = ls1mcs_store:add_unknown_frame(Binary, erlang:now());

log_unkn_frame(_Binary, false) ->
    ok.


%%
%%  LS1P encoder.
%%

%%  Acknowledgement frame.
encode(Frame, _Password) when is_record(Frame, ls1p_ack_frame) ->
    #ls1p_ack_frame{
        status = Status,
        cref = {_Epoch, CRef},
        recv_status = RecvStatus
    } = Frame,
    StatusBin = encode_bool(Status),
    FrameBin = <<?GROUND_ADDR:3, ?GROUND_PORT_ACK:4, StatusBin:1, CRef:16/little, RecvStatus:8>>,
    {ok, FrameBin};

%%  Data frame.
encode(Frame, _Password) when is_record(Frame, ls1p_data_frame) ->
    #ls1p_data_frame{
        eof = Eof,
        cref = {_Epoch, CRef},
        fragment = Fragment,
        data = Data
    } = Frame,
    EofBin = encode_bool(Eof),
    FrameBin = <<?GROUND_ADDR:3, ?GROUND_PORT_DATA:4, EofBin:1, CRef:16/little, Fragment:16/little, Data/binary>>,
    {ok, FrameBin};

%%  Telemetry frame.
encode(Frame, _Password) when is_record(Frame, ls1p_tm_frame) ->
    #ls1p_tm_frame{
        data = Telemetry
    } = Frame,
    FrameBin = <<?GROUND_ADDR:3, ?GROUND_PORT_TM:4, 0:1, Telemetry/binary>>,
    {ok, FrameBin};

%%  Command frame.
encode(Frame, Password) when is_record(Frame, ls1p_cmd_frame) ->
    #ls1p_cmd_frame{
        addr = Addr, port = Port, ack = Ack,
        cref = {_Epoch, CRef}, delay = Delay, data = Data
    } = Frame,
    AddrBin = encode_addr(Addr),
    PortBin = encode_port(Addr, Port),
    AckBin  = encode_bool(Ack),
    FrameBin = <<AddrBin:3, PortBin:4, AckBin:1, CRef:16/little, Delay:16/little, Data/binary>>,
    sign_command(FrameBin, Password).


%%
%%  LS1P decoder.
%%

%%  Acknowledgement frame.
decode(<<?GROUND_ADDR:3, ?GROUND_PORT_ACK:4, StatusBin:1, CRef:16/little, RecvStatus:8>>) ->
    Status = decode_bool(StatusBin),
    Frame = #ls1p_ack_frame{
        status = Status,
        cref = {undefined, CRef},
        recv_status = RecvStatus
    },
    {ok, Frame};

%%  Data frame
decode(<<?GROUND_ADDR:3, ?GROUND_PORT_DATA:4, EofBin:1, CRef:16/little, Fragment:16/little, Data/binary>>) ->
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
decode(<<AddrBin:3, PortBin:4, AckBin:1, CRef:16/little, Delay:16/little, Data/binary>>) ->
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


%%
%%  Signs a command.
%%
sign_command(CommandFrame, undefined) ->
    {ok, CommandFrame};

sign_command(CommandFrame, Password) ->
    {ok, <<CkA, CkB>>} = ls1mcs_utl_cksum:checksum(CommandFrame, fletcher8bit),
    <<PwA, PwB>> = Password,
    Signature = <<(CkA bxor PwA):8, (CkB bxor PwB):8>>,
    <<
        S00:1, S01:1, S02:1, S03:1, S04:1, S05:1, S06:1, S07:1,
        S08:1, S09:1, S10:1, S11:1, S12:1, S13:1, S14:1, S15:1
    >> = Signature,
    <<
        F00:1, F01:1, F02:1, F03:1, F04:1, F05:1, F06:1, F07:1,
        F08:1, F09:1, F10:1, F11:1, F12:1, F13:1, F14:1, F15:1,
        FrameTail/binary
    >> = CommandFrame,
    FrameWithSignature = <<
        S00:1, F00:1, S01:1, F01:1, S02:1, F02:1, S03:1, F03:1,
        S04:1, F04:1, S05:1, F05:1, S06:1, F06:1, S07:1, F07:1,
        S08:1, F08:1, S09:1, F09:1, S10:1, F10:1, S11:1, F11:1,
        S12:1, F12:1, S13:1, F13:1, S14:1, F14:1, S15:1, F15:1,
        FrameTail/binary
    >>,
    {ok, FrameWithSignature}.


%%
%%  Checks a command signature.
%%  This function is needed here to recognize echoed commands properly.
%%
check_signature(FrameWithSignature, undefined) ->
    {ok, FrameWithSignature};

check_signature(FrameWithSignature, _Password) when size(FrameWithSignature) < 4 ->
    {error, badframe};

check_signature(FrameWithSignature, Password) ->
    <<
        S00:1, F00:1, S01:1, F01:1, S02:1, F02:1, S03:1, F03:1,
        S04:1, F04:1, S05:1, F05:1, S06:1, F06:1, S07:1, F07:1,
        S08:1, F08:1, S09:1, F09:1, S10:1, F10:1, S11:1, F11:1,
        S12:1, F12:1, S13:1, F13:1, S14:1, F14:1, S15:1, F15:1,
        FrameTail/binary
    >> = FrameWithSignature,
    SuppliedSignature = <<
        S00:1, S01:1, S02:1, S03:1, S04:1, S05:1, S06:1, S07:1,
        S08:1, S09:1, S10:1, S11:1, S12:1, S13:1, S14:1, S15:1
    >>,
    CommandFrame = <<
        F00:1, F01:1, F02:1, F03:1, F04:1, F05:1, F06:1, F07:1,
        F08:1, F09:1, F10:1, F11:1, F12:1, F13:1, F14:1, F15:1,
        FrameTail/binary
    >>,
    {ok, <<CkA, CkB>>} = ls1mcs_utl_cksum:checksum(CommandFrame, fletcher8bit),
    <<PwA, PwB>> = Password,
    CalculatedSignature = <<(CkA bxor PwA):8, (CkB bxor PwB):8>>,
    case SuppliedSignature =:= CalculatedSignature of
        true ->
            {ok, CommandFrame};
        false ->
            {error, badsign}
    end.



%% =============================================================================
%%  Telemetry decoder.
%% =============================================================================



%%
%%  Decode entire telemetry frame (without frame header byte).
%%  Total length: 229 bytes
%%
decode_tm(Telemetry) when is_binary(Telemetry), size(Telemetry) =:= 229 ->
    <<
        Time:(4*8)/little,  %% 4 bytes, centi-seconds (1s / 100).
        Hk:60/binary,       %% Housekeeping data
        Att1:55/binary,     %% Attitude read-1
        Att2:55/binary,     %% Attitude read-2
        Att3:55/binary      %% Attitude read-3
    >> = Telemetry,
    #tm{
        time = Time / 100,
        hk = decode_tm_hk(Hk),
        att = [
            decode_tm_att(Att1),
            decode_tm_att(Att2),
            decode_tm_att(Att3)
        ]
    };

decode_tm(Telemetry) ->
    lager:warning("Unknown TM structure, bytes: ~p", [Telemetry]),
    #tm{}.


%%
%%  Decode housekeeping data.
%%  Total length: 60 bytes
%%
decode_tm_hk(Telemetry) ->
    <<
        PwrMode:4,          %% Power management mode
        SatMode:4,          %% Sat mode
        EPS:43/binary,      %% EPS
        He:16/binary        %% Helium-100
    >> = Telemetry,
    PwrModeFun = fun
        (0) -> safe;
        (1) -> nominal;
        (_) -> unknown
    end,
    SatModeFun = fun
        (0) -> idle;
        (1) -> nominal;
        (2) -> science;
        (_) -> unknown
    end,
    #tm_hk{
        pwr_mode = PwrModeFun(PwrMode),
        sat_mode = SatModeFun(SatMode),
        eps = decode_tm_eps(EPS),
        he = decode_tm_he(He)
    }.


%%
%%  Decode attitude data.
%%  Total length: 55 bytes
%%
decode_tm_att(Telemetry) ->
    <<
        HMC5883L_mag:7/binary,      %% inc/Sensors.h: mag_report
        MPU6000A_accel:7/binary,    %% inc/Sensors.h: accel_report
        MPU6000A_gyro:9/binary,     %% inc/Sensors.h: gyro_report
        MPU9150A_accel:7/binary,    %% inc/Sensors.h: accel_report
        MPU9150A_gyro:9/binary,     %% inc/Sensors.h: gyro_report
        AK8975_mag:7/binary,        %% inc/Sensors.h: mag_report
        L3GD20_gyro:9/binary        %% inc/Sensors.h: gyro_report
    >> = Telemetry,
    #tm_att{
        s_HMC5883L_mag   = decode_tm_mag  (HMC5883L_mag,   'HMC5883L'),
        s_MPU6000A_accel = decode_tm_accel(MPU6000A_accel, 'MPU6000A'),
        s_MPU6000A_gyro  = decode_tm_gyro (MPU6000A_gyro,  'MPU6000A'),
        s_MPU9150A_accel = decode_tm_accel(MPU9150A_accel, 'MPU9150A'),
        s_MPU9150A_gyro  = decode_tm_gyro (MPU9150A_gyro,  'MPU9150A'),
        s_AK8975_mag     = decode_tm_mag  (AK8975_mag,     'AK8975'),
        s_L3GD20_gyro    = decode_tm_gyro (L3GD20_gyro,    'L3GD20')
    }.

%%
%%  Decode P31U (power unit) data.
%%  Total length: 43 bytes.
%%
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
        PV1:16/little,
        PV2:16/little,
        PV3:16/little,
        PC:16/little,
        BV:16/little,
        SC:16/little,
        TempBC1:16/little-signed,
        TempBC2:16/little-signed,
        TempBC3:16/little-signed,
        TempOB:16/little-signed,
        BattTemp1:16/little-signed,
        BattTemp2:16/little-signed,
        Latchup50V1:16/little,
        Latchup50V2:16/little,
        Latchup50V3:16/little,
        Latchup33V1:16/little,
        Latchup33V2:16/little,
        Latchup33V3:16/little,
        Reset:8,
        Bootcount:16/little,
        SwErrors:16/little,
        PPTMode:8,
        ChannelStatus50V1:1,
        ChannelStatus50V2:1,
        ChannelStatus50V3:1,
        ChannelStatus33V1:1,
        ChannelStatus33V2:1,
        ChannelStatus33V3:1,
        ChannelStatusQS:1,
        ChannelStatusQH:1
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
%%  Decode Helium-100 (transceiver) data.
%%  Total length: 16 bytes.
%%
decode_tm_he(Telemetry) ->
    <<
        OpCounter:16/little,
        MSP430Temp:16/little-signed,
        TimeCount1:8,
        TimeCount2:8,
        TimeCount3:8,
        RSSI:8,
        BytesReceived:32/little,
        BytesTransmitted:32/little
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
%%  Decode magnetometer data.
%%  Total length: 7 bytes.
%%
decode_tm_mag(Telemetry, Sensor) ->
    <<
        X:16/little-signed,
        Y:16/little-signed,
        Z:16/little-signed,
        Gain:8
    >> = Telemetry,
    Coef_mG_to_uT = 0.1,   % Converts mili Gauses to micro Teslas.
    Coef = case Sensor of
        'HMC5883L' -> lists:nth(Gain + 1, [0.73, 0.92, 1.22, 1.52, 2.27, 2.56, 3.03, 4.35]) * Coef_mG_to_uT;
        'AK8975'   -> lists:nth(Gain + 1, [0.3])
    end,
    #tm_mag{
        x = X * Coef,
        y = Y * Coef,
        z = Z * Coef
    }.


%%
%%  Decode accelerometer data.
%%  Total length: 7 bytes.
%%
decode_tm_accel(Telemetry, Sensor) ->
    <<
        X:16/little-signed,
        Y:16/little-signed,
        Z:16/little-signed,
        Gain:8
    >> = Telemetry,
    MPUCoefs = [6.1035156E-05, 1.2207031E-04, 2.4414063E-04, 4.8828125E-04],
    Coef = case Sensor of
        'MPU6000A' -> lists:nth(Gain + 1, MPUCoefs);
        'MPU9150A' -> lists:nth(Gain + 1, MPUCoefs)
    end,
    #tm_accel{
        x = X * Coef,
        y = Y * Coef,
        z = Z * Coef
    }.



%%
%%  Gyroscope and temperature data.
%%  Total length: 9 bytes.
%%
decode_tm_gyro(Telemetry, Sensor) ->
    <<
        X:16/little-signed,
        Y:16/little-signed,
        Z:16/little-signed,
        Temp:16/little-signed,
        Gain:8
    >> = Telemetry,
    MPUCoefs = [7.6335878E-03, 1.5267176E-02, 3.0487805E-02, 6.0975610E-02],
    L3GCoefs = [1.5258789E-02, 3.0517578E-02, 6.1035156E-02, 6.1035156E-02],
    Coef = case Sensor of
        'MPU6000A' -> lists:nth(Gain + 1, MPUCoefs);
        'MPU9150A' -> lists:nth(Gain + 1, MPUCoefs);
        'L3GD20'   -> lists:nth(Gain + 1, L3GCoefs)
    end,
    #tm_gyro{
        x = X * Coef,
        y = Y * Coef,
        z = Z * Coef,
        temp = case Sensor of
            'MPU6000A' -> Temp / 340 + 35;
            'MPU9150A' -> Temp / 340 + 35;
            'L3GD20'   -> Temp * 1.0
        end
    }.



%% =============================================================================
%%  Data reconstruction.
%% =============================================================================


%%
%%  Decodes photo metadata, returns its fields.
%%
-spec decode_photo_meta(binary()) -> {ok, Size :: integer()}.

decode_photo_meta(Binary) ->
    <<
        PhotoSize:16/little,
        _CRef:16/little     % Not used not, should be zeros always.
    >> = Binary,
    {ok, PhotoSize}.


%%
%%
%%
-spec merged_response_fragments(
        [{#ls1p_cmd_frame{}, [#ls1p_data_frame{}]}]
        ) ->
        {ok, [{From :: integer(), Till :: integer(), Data :: binary()}]}.

merged_response_fragments(Commands) ->
    BlockSets = [ merge_to_blocks(C, D) || {C, D} <- Commands ],
    MergedBlocks = lists:foldl(fun overlay_block_sets/2, [], BlockSets),
    {ok, MergedBlocks}.


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
    {ok, MergedBlocks} = merged_response_fragments(Commands),
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
    PrefixSize = OFrom - BFrom,
    OverlappedSize = OTill - OFrom,
    <<Prefix:PrefixSize/binary, _:OverlappedSize/binary, Suffix/binary>> = BData,
    {BFrom, BTill, <<Prefix/binary, OData/binary, Suffix/binary>>};

% Overlay block covers front of the base block (or touches it).
overlay({OFrom, OTill, OData}, {BFrom, BTill, BData}) when OFrom =< BFrom ->
    OverlappedSize = OTill - BFrom,
    <<_:OverlappedSize/binary, Suffix/binary>> = BData,
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
%%  TODO: Sort by time desc explicitly. WTF?
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
    <<BlkSz:8, From:16/little, Till:16/little>> = Data,
    {ok, BlkSz, From, Till};

fragment_metainfo(#ls1p_cmd_frame{addr = arm, port = downlink, data = Data}) ->
    <<_BufId:8, BlkSz:8, From:16/little, Till:16/little>> = Data,
    {ok, BlkSz, From, Till}.



%%
%%
%%
-spec merged_archive_fragments(
        [{#ls1p_cmd_frame{}, [#ls1p_data_frame{}]}]
        ) ->
        {ok, [{From :: integer(), Till :: integer(), Data :: binary()}]}.

merged_archive_fragments(CommandsWithData) ->
    BlockSets = [ merge_to_blocks(C, D) || {C, D} <- CommandsWithData ],

    % TODO: Implement overlay in a different way.

    MergedBlocks = lists:foldl(fun overlay_block_sets/2, [], BlockSets),
    {ok, MergedBlocks}.

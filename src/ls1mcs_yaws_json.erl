-module(ls1mcs_yaws_json).
-export([encode_list/1, encode/1]).
-include("ls1mcs.hrl").


%%
%%
%%
encode_list(List) ->
    [ encode(E) || E <- List ].


%%
%%
%%
encode(#command_address{desc = Desc, addr = Addr}) ->
    {[
        {desc, Desc},
        {addr, Addr}
    ]};

encode(#user_cmd_spec{desc = Desc, addr = Addr, port = Port, ack = Ack, comp = Comp, params = Params}) ->
    {[
        {desc, Desc},
        {addr, Addr},
        {port, Port},
        {ack, Ack},
        {comp, Comp},
        {params, encode_list(Params)}
    ]};

encode(#user_cmd_param{name = Name, desc = Desc, type = Type, enum = Enum}) ->
    EnumEncoded = case Enum of
        undefined -> [];
        _ -> [ {enum, encode_list(Enum)} ]
    end,
    {[
        {name, Name},
        {desc, Desc},
        {type, Type}
    ] ++ EnumEncoded};

encode(#user_cmd_enum{desc = Desc, value = Value}) ->
    {[
        {desc, Desc},
        {value, Value}
    ]};

%% -----------------------------------------------------------------------------
%%  Telemetry
%% -----------------------------------------------------------------------------

encode(#ls1p_tm_frame{data = Data}) ->
    encode(ls1mcs_proto_ls1p:decode_tm(Data));

encode(#tm{time = Time, eps = EPS, he = He, att = Att}) ->
    {[
        {time, Time},
        {eps, encode(EPS)},
        {he, encode(He)},
        {att, encode_list(Att)}
    ]};

encode(#tm_att{mag = Mag, mpu = MPU, gyro_1 = Gyro1, gyro_2 = Gyro2}) ->
    {[
        {mag, encode(Mag)},
        {mpu, encode(MPU)},
        {gyro_1, encode(Gyro1)},
        {gyro_2, encode(Gyro2)}
    ]};

encode(TmEps) when is_record(TmEps, tm_eps) ->
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
        ppt_mode        = PPTMode,
        channel_status_QH   = ChannelStatusQH,
        channel_status_QS   = ChannelStatusQS,
        channel_status_50V1 = ChannelStatus50V1,
        channel_status_50V2 = ChannelStatus50V2,
        channel_status_50V3 = ChannelStatus50V3,
        channel_status_33V1 = ChannelStatus33V1,
        channel_status_33V2 = ChannelStatus33V2,
        channel_status_33V3 = ChannelStatus33V3
    } = TmEps,
    {[
        {pv_1, PV1},
        {pv_2, PV2},
        {pv_3, PV3},
        {pc, PC},
        {bv, BV},
        {sc, SC},
        {temp_BC1, TempBC1},
        {temp_BC2, TempBC2},
        {temp_BC3, TempBC3},
        {temp_OB, TempOB},
        {batt_temp_1, BattTemp1},
        {batt_temp_2, BattTemp2},
        {latchup_50V1, Latchup50V1},
        {latchup_50V2, Latchup50V2},
        {latchup_50V3, Latchup50V3},
        {latchup_33V1, Latchup33V1},
        {latchup_33V2, Latchup33V2},
        {latchup_33V3, Latchup33V3},
        {reset, Reset},
        {bootcount, Bootcount},
        {sw_errors, SwErrors},
        {ppt_mode, PPTMode},
        {channel_status_QH, ChannelStatusQH},
        {channel_status_QS, ChannelStatusQS},
        {channel_status_50V1, ChannelStatus50V1},
        {channel_status_50V2, ChannelStatus50V2},
        {channel_status_50V3, ChannelStatus50V3},
        {channel_status_33V1, ChannelStatus33V1},
        {channel_status_33V2, ChannelStatus33V2},
        {channel_status_33V3, ChannelStatus33V3}
    ]};

encode(TmHe) when is_record(TmHe, tm_he) ->
    #tm_he{
        op_counter = OpCounter,
        msp430_temp = MSP430Temp,
        time_count_1 = TimeCount1,
        time_count_2 = TimeCount2,
        time_count_3 = TimeCount3,
        rssi = RSSI,
        bytes_received = BytesReceived,
        bytes_transmitted = BytesTransmitted
    } = TmHe,
    {[
        {op_counter, OpCounter},
        {msp430_temp, MSP430Temp},
        {time_count_1, TimeCount1},
        {time_count_2, TimeCount2},
        {time_count_3, TimeCount3},
        {rssi, RSSI},
        {bytes_received, BytesReceived},
        {bytes_transmitted, BytesTransmitted}
    ]};

encode(#tm_mag{bx = Bx, by = By, bz = Bz}) ->
    {[
        {bx, Bx},
        {by, By},
        {bz, Bz}
    ]};

encode(#tm_mpu{gx = Gx, gy = Gy, gz = Gz, ax = Ax, ay = Ay, az = Az, temp = Temp}) ->
    {[
        {gx, Gx},
        {gy, Gy},
        {gz, Gz},
        {ax, Ax},
        {ay, Ay},
        {az, Az},
        {temp, Temp}
    ]};

encode(#tm_gyro{wx = Wx, wy = Wy, wz = Wz, temp = Temp}) ->
    {[
        {wx, Wx},
        {wy, Wy},
        {wz, Wz},
        {temp, Temp}
    ]}.



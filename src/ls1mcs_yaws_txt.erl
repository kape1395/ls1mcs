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
%%  Example: curl http://localhost:8000/ls1mcs/api/telemetry/gs?t=txt
%%
-module(ls1mcs_yaws_txt).
-export([encode/1, encode_list/1]).
-include("ls1mcs.hrl").


%% =============================================================================
%%  Encoder.
%% =============================================================================


%% -----------------------------------------------------------------------------
%%  Telemetry
%% -----------------------------------------------------------------------------
encode(undefined) ->
    [];

encode(#ls1p_tm_frame{id = Id, recv = Recv, data = Data}) ->
    #tm{time = Time, hk = Hk, att = Att} = ls1mcs_proto_ls1p:decode_tm(Data),
    case Time of
        undefined ->
            [
                "# Invalid frame structure id=",
                encode_number(Id)
            ];
        _ ->
            [
                encode_number(Id), $\t,
                encode_tstamp(Recv), $\t,
                encode_number(Time), $\t,
                encode(Hk), $\t,
                encode_list(Att)
            ]
    end;

encode(#tm_hk{
        pwr_mode = PwrMode,
        sat_mode = SatMode,
        eps = EPS,
        he = He
    }) ->
    [
        encode_number(PwrMode), $\t,
        encode_number(SatMode), $\t,
        encode(EPS), $\t,
        encode(He)
    ];

encode(#tm_att{
        s_HMC5883L_mag   = HMC5883L_mag,
        s_MPU6000A_accel = MPU6000A_accel,
        s_MPU6000A_gyro  = MPU6000A_gyro,
        s_MPU9150A_accel = MPU9150A_accel,
        s_MPU9150A_gyro  = MPU9150A_gyro,
        s_AK8975_mag     = AK8975_mag,
        s_L3GD20_gyro    = L3GD20_gyro
    }) ->
    [
        encode(HMC5883L_mag), $\t,
        encode(MPU6000A_accel), $\t,
        encode(MPU6000A_gyro), $\t,
        encode(MPU9150A_accel), $\t,
        encode(MPU9150A_gyro), $\t,
        encode(AK8975_mag), $\t,
        encode(L3GD20_gyro)
    ];

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
    [
        encode_number(PV1), $\t,
        encode_number(PV2), $\t,
        encode_number(PV3), $\t,
        encode_number(PC), $\t,
        encode_number(BV), $\t,
        encode_number(SC), $\t,
        encode_number(TempBC1), $\t,
        encode_number(TempBC2), $\t,
        encode_number(TempBC3), $\t,
        encode_number(TempOB), $\t,
        encode_number(BattTemp1), $\t,
        encode_number(BattTemp2), $\t,
        encode_number(Latchup50V1), $\t,
        encode_number(Latchup50V2), $\t,
        encode_number(Latchup50V3), $\t,
        encode_number(Latchup33V1), $\t,
        encode_number(Latchup33V2), $\t,
        encode_number(Latchup33V3), $\t,
        encode_number(Reset), $\t,
        encode_number(Bootcount), $\t,
        encode_number(SwErrors), $\t,
        encode_number(PPTMode), $\t,
        encode_number(ChannelStatusQH), $\t,
        encode_number(ChannelStatusQS), $\t,
        encode_number(ChannelStatus50V1), $\t,
        encode_number(ChannelStatus50V2), $\t,
        encode_number(ChannelStatus50V3), $\t,
        encode_number(ChannelStatus33V1), $\t,
        encode_number(ChannelStatus33V2), $\t,
        encode_number(ChannelStatus33V3)
    ];

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
    [
        encode_number(OpCounter), $\t,
        encode_number(MSP430Temp), $\t,
        encode_number(TimeCount1), $\t,
        encode_number(TimeCount2), $\t,
        encode_number(TimeCount3), $\t,
        encode_number(RSSI), $\t,
        encode_number(BytesReceived), $\t,
        encode_number(BytesTransmitted)
    ];

encode(#tm_mag{x = X, y = Y, z = Z}) ->
    [
        encode_number(X), $\t,
        encode_number(Y), $\t,
        encode_number(Z)
    ];

encode(#tm_accel{x = X, y = Y, z = Z}) ->
    [
        encode_number(X), $\t,
        encode_number(Y), $\t,
        encode_number(Z)
    ];

encode(#tm_gyro{x = X, y = Y, z = Z, temp = T}) ->
    [
        encode_number(X), $\t,
        encode_number(Y), $\t,
        encode_number(Z), $\t,
        encode_number(T)
    ];

encode(#usr_cmd{id = Id, spec = Spec, args = Args, issued = Issued, status = Status}) ->
    EncodeArg = fun (#usr_cmd_arg{name = Name, value = Value}) ->
        [
            encode_string(Name), <<"=">>,
            encode_string(Value), <<" ">>
        ]
    end,
    [
        encode_number(Id), $\t,
        encode_string(Spec), $\t,
        case Args of
            undefined -> <<>>;
            _ -> lists:map(EncodeArg, Args)
        end, $\t,
        encode_tstamp(Issued), $\t,
        encode_string(Status), $\t
    ].


%%
%%  Encode list of entities.
%%
encode_list(undefined) ->
    [];

encode_list([]) ->
    [];

encode_list([First | Others]) when is_record(First, tm_att) ->
    [
        encode(First) |
        [ [$\t, encode(E)] || E <- Others ]
    ];

encode_list([First | Others]) when is_record(First, ls1p_tm_frame) ->
    HdrEPS = #tm_eps{
        pv_1 = pv_1,
        pv_2 = pv_2,
        pv_3 = pv_3,
        pc = pc,
        bv = bv,
        sc = sc,
        temp_BC1 = temp_BC1,
        temp_BC2 = temp_BC2,
        temp_BC3 = temp_BC3,
        temp_OB = temp_OB,
        batt_temp_1 = batt_temp_1,
        batt_temp_2 = batt_temp_2,
        latchup_50V1 = latchup_50V1,
        latchup_50V2 = latchup_50V2,
        latchup_50V3 = latchup_50V3,
        latchup_33V1 = latchup_33V1,
        latchup_33V2 = latchup_33V2,
        latchup_33V3 = latchup_33V3,
        reset           = reset,
        bootcount       = bootcount,
        sw_errors       = sw_errors,
        ppt_mode        = ppt_mode,
        channel_status_QH   = channel_status_QH,
        channel_status_QS   = channel_status_QS,
        channel_status_50V1 = channel_status_50V1,
        channel_status_50V2 = channel_status_50V2,
        channel_status_50V3 = channel_status_50V3,
        channel_status_33V1 = channel_status_33V1,
        channel_status_33V2 = channel_status_33V2,
        channel_status_33V3 = channel_status_33V3
    },
    HdrHe = #tm_he{
        op_counter = op_counter,
        msp430_temp = msp430_temp,
        time_count_1 = time_count_1,
        time_count_2 = time_count_2,
        time_count_3 = time_count_3,
        rssi = rssi,
        bytes_received = bytes_received,
        bytes_transmitted = bytes_transmitted
    },
    HdrAtt = #tm_att{
        s_HMC5883L_mag = #tm_mag{
            x = 'HMC5883L_mag_x',
            y = 'HMC5883L_mag_y',
            z = 'HMC5883L_mag_z'
        },
        s_MPU6000A_accel = #tm_accel{
            x = 'MPU6000A_accel_x',
            y = 'MPU6000A_accel_y',
            z = 'MPU6000A_accel_z'
        },
        s_MPU6000A_gyro = #tm_gyro {
            x = 'MPU6000A_gyro_x',
            y = 'MPU6000A_gyro_y',
            z = 'MPU6000A_gyro_z',
            temp = 'MPU6000A_gyro_temp'
        },
        s_MPU9150A_accel = #tm_accel{
            x = 'MPU9150A_accel_x',
            y = 'MPU9150A_accel_y',
            z = 'MPU9150A_accel_z'
        },
        s_MPU9150A_gyro = #tm_gyro{
            x = 'MPU9150A_gyro_x',
            y = 'MPU9150A_gyro_y',
            z = 'MPU9150A_gyro_z',
            temp = 'MPU9150A_gyro_temp'
        },
        s_AK8975_mag = #tm_mag{
            x = 'AK8975_mag_x',
            y = 'AK8975_mag_y',
            z = 'AK8975_mag_z'
        },
        s_L3GD20_gyro = #tm_gyro{
            x = 'L3GD20_gyro_x',
            y = 'L3GD20_gyro_y',
            z = 'L3GD20_gyro_z',
            temp = 'L3GD20_gyro_temp'
        }
    },
    Header = erlang:iolist_to_binary([
        <<"# id\t">>,
        <<"recv\t">>,
        <<"time\t">>,
        encode(#tm_hk{
            pwr_mode = pwr_mode,
            sat_mode = sat_mode,
            eps = HdrEPS,
            he = HdrHe
        }),
        $\t,
        encode_list([HdrAtt, HdrAtt, HdrAtt]),
        $\n
    ]),
    [
        Header |
        [ iolist_to_binary([encode(E), $\n]) || E <- Others ]
    ];

encode_list([First | Others]) when is_record(First, usr_cmd) ->
    Header = erlang:iolist_to_binary([
        <<"# id\t">>,
        <<"spec\t">>,
        <<"args\t">>,
        <<"issued\t">>,
        <<"status\n">>
    ]),
    [
        Header |
        [ iolist_to_binary([encode(E), $\n]) || E <- [First | Others] ]
    ].



%%
%%  Encode timestamp.
%%
encode_tstamp(undefined) ->
    null;

encode_tstamp({{Y, M, D}, {H, Mi, S}}) ->
    Args = [Y, M, D, H, Mi, S],
    Date = io_lib:format("~B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ", Args),
    erlang:iolist_to_binary(Date);

encode_tstamp(Now = {_MegaSecs, _Secs, MicroSecs}) ->
    {{Y, M, D}, {H, Mi, S}} = calendar:now_to_datetime(Now),
    Args = [Y, M, D, H, Mi, S, MicroSecs],
    Date = io_lib:format("~B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0BZ", Args),
    erlang:iolist_to_binary(Date).


%%
%%
%%
encode_number(undefined) ->
    <<>>;

encode_number(EnumVal) when is_atom(EnumVal) ->
    erlang:atom_to_binary(EnumVal, utf8);

encode_number(Header) when is_binary(Header) ->
    Header;

encode_number(Number) when is_integer(Number) ->
    erlang:integer_to_binary(Number);

encode_number(Number) when is_float(Number) ->
    erlang:float_to_binary(Number, [{decimals, 6}, compact]).


%%
%%
%%
encode_string(undefined) ->
    <<>>;

encode_string(String) when is_binary(String) ->
    String;

encode_string(String) when is_atom(String) ->
    erlang:atom_to_binary(String, utf8);

encode_string(String) when is_list(String) ->
    erlang:list_to_binary(String).




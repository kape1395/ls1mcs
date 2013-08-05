-module(ls1mcs_yaws_json).
-export([encode/1, encode_self/2, encode_list/1, encode_tstamp/1, decode/2, decode_atom/1, decode_integer/1]).
-include("ls1mcs.hrl").


%% =============================================================================
%%  Encoder.
%% =============================================================================

%%
%%
%%
encode({root}) ->
    {[
        links([
            link(self,                  url([])),
            link(command,               url([command])),
            link(ls1p_frames,           url([ls1p_frame])),
            link(telemetry,             url([telemetry])),
            link(sats,                  url([sat]))
        ])
    ]};


%% -----------------------------------------------------------------------------
%%  Commands
%% -----------------------------------------------------------------------------

encode({command}) ->
    {[
        links([
            link(self,      url([command])),
            link(addrs,     url([command, group])),     % User command addresses
            link(specs,     url([command, spec])),      % User command specs
            link(usr_cmds,  url([command, usr])),       % User commands
            link(sat_cmds,  url([command, sat])),       % SAT commands
            link(plans,     url([command, plan])),      % Plans
            link(immediate, url([command, immediate]))  % Immediate commands
        ])
    ]};


encode(#usr_cmd_group{desc = Desc, name = Name}) ->
    {[
        {desc, Desc},
        {name, Name}
    ]};

encode(#usr_cmd_spec{name = Name, desc = Desc, group = Group, params = Params}) ->
    {[
        {name, Name},
        {desc, Desc},
        {group, Group},
        {params, encode_list(Params)}
    ]};

encode(#usr_cmd_param{name = Name, desc = Desc, type = Type, opts = Opts}) ->
    OptsEncoded = case Opts of
        undefined -> [];
        _ -> [ {opts, encode_list(Opts)} ]
    end,
    {[
        {name, Name},
        {desc, Desc},
        {type, Type}
    ] ++ OptsEncoded};

encode(#usr_cmd_opts{desc = Desc, value = Value}) ->
    {[
        {desc, Desc},
        {value, Value}
    ]};

encode(#usr_cmd_arg{name = Name, value = Value}) ->
    {[
        {name, Name},
        {value, Value}
    ]};

encode(#usr_cmd{
        id = Id,
        spec = Spec,
        args = Args,
        immediate = Immediate,
        approved = Approved,
        issued = Issued,
        status = Status
    }) ->
    {[
        {id, Id},
        {spec, Spec},
        {args, encode_list(Args)},
        {immediate, Immediate},
        {approved, encode_tstamp(Approved)},
        {issued, encode_tstamp(Issued)},
        {status, Status}
    ]};

encode(#sat_cmd{
        id = Id,
        usr_cmd_id = UsrCmdId,
        cmd_frame = CmdFrame,
        acked = Acked,
        executed = Executed,
        dat_recv = DatRecv,
        eof_recv = EofRecv,
        log_recv = LogRecv
    }) ->
    {[
        {id, Id},
        {usr_cmd_id, UsrCmdId},
        {cmd_frame, encode(CmdFrame)},
        {acked, encode_tstamp(Acked)},
        {executed, encode_tstamp(Executed)},
        {dat_recv, encode_tstamp(DatRecv)},
        {eof_recv, encode_tstamp(EofRecv)},
        {log_recv, encode_tstamp(LogRecv)}
    ]};

%% -----------------------------------------------------------------------------
%%  LS1P Frames
%% -----------------------------------------------------------------------------

encode({cref, undefined}) ->
    null;

encode({cref, {Epoch, CRef}}) ->
    EpochBin = erlang:integer_to_binary(Epoch),
    CRefBin = erlang:integer_to_binary(CRef),
    <<EpochBin/binary, "_", CRefBin/binary>>;

encode({cref, {Epoch, CRef, {MegaSec, Sec, MicroSec}}}) ->
    EpochBin    = erlang:integer_to_binary(Epoch),
    CRefBin     = erlang:integer_to_binary(CRef),
    MegaSecBin  = erlang:integer_to_binary(MegaSec),
    SecBin      = erlang:integer_to_binary(Sec),
    MicroSecBin = erlang:integer_to_binary(MicroSec),
    <<EpochBin/binary, "_", CRefBin/binary, "_", MegaSecBin/binary, "_", SecBin/binary, "_", MicroSecBin/binary>>;

encode(#ls1p_cmd_frame{
        addr = Addr,
        port = Port,
        ack = Ack,
        cref = CRef,
        delay = Delay,
        data = Data
    }) ->
    {[
        {addr, Addr},
        {port, Port},
        {ack, Ack},
        {cref, encode({cref, CRef})},
        {delay, Delay},
        {data, encode_hex(Data)}
    ]};

encode(#ls1p_ack_frame{
        status = Status,
        cref = CRef,
        recv_status = RecvStatus
    }) ->
    {[
        {status, Status},
        {cref, encode({cref, CRef})},
        {recv_status, RecvStatus}
    ]};

encode(#ls1p_data_frame{
        eof = Eof,
        cref = CRef,
        fragment = Fragment,
        data = Data
    }) ->
    {[
        {eof, Eof},
        {cref, encode({cref, CRef})},
        {fragment, Fragment},
        {data, encode_hex(Data)}
    ]};


%% -----------------------------------------------------------------------------
%%  Telemetry
%% -----------------------------------------------------------------------------

encode({telemetry}) ->
    {[
        links([
            link(self,      url([telemetry])),
            link(latest,    url([telemetry, gs, latest])),
            link(gs,        url([telemetry, gs])),
            link(ham,       url([telemetry, ham])),
            link(archive,   url([telemetry, archive]))
        ])
    ]};

encode(#ls1p_tm_frame{id = Id, recv = Recv, data = Data}) ->
    #tm{time = Time, eps = EPS, he = He, att = Att} = ls1mcs_proto_ls1p:decode_tm(Data),
    {[
        links([
            link(self, url(["telemetry", Id]))
        ]),
        {id, Id},
        {recv, encode_tstamp(Recv)},
        {time, Time},
        {eps, encode(EPS)},
        {he, encode(He)},
        {att,
            case is_list(Att) of
                true -> encode_list(Att);
                false -> encode(Att)
            end
        }
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


%%
%%
%%
encode_self(usr_cmd, Id) ->
    {[ links([link(self, url([command, usr, Id]))]) ]}.


%%
%%  Encode list of entities.
%%
encode_list(undefined) ->
    null;

encode_list(List) ->
    [ encode(E) || E <- List ].


%%
%%  Encode binary to HEX string.
%%
encode_hex(Binary) ->
    erlang:iolist_to_binary([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Binary)]).


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




%% =============================================================================
%%  Decoder.
%% =============================================================================

decode(cref, CRef) when is_list(CRef) ->
    decode(cref, erlang:list_to_binary(CRef));

decode(cref, CRef) when is_binary(CRef) ->
    case binary:split(CRef, <<"_">>, [global]) of
        [EpochBin, CRefBin] ->
            {
                erlang:binary_to_integer(EpochBin),
                erlang:binary_to_integer(CRefBin)
            };
        [EpochBin, CRefBin, MegaSecBin, SecBin, MicroSecBin] ->
            {
                erlang:binary_to_integer(EpochBin),
                erlang:binary_to_integer(CRefBin),
                {
                    erlang:binary_to_integer(MegaSecBin),
                    erlang:binary_to_integer(SecBin),
                    erlang:binary_to_integer(MicroSecBin)
                }
            }

    end;

decode(usr_cmd, {PL}) ->
    #usr_cmd{
        id          = decode_integer(proplists:get_value(<<"id">>, PL)),
        spec        = decode_atom(proplists:get_value(<<"spec">>, PL)),
        args        = decode_list(usr_cmd_arg, proplists:get_value(<<"args">>, PL)),
        immediate   = undefined,
        approved    = undefined,
        issued      = undefined,
        status      = undefined
    };

decode(usr_cmd_arg, {PL}) ->
    #usr_cmd_arg{
        name    = decode_atom(proplists:get_value(<<"name">>, PL)),
        value   = decode_binary(proplists:get_value(<<"value">>, PL))
    }.


%%
%%
%%
decode_list(_Type, null) ->
    undefined;

decode_list(_Type, undefined) ->
    undefined;

decode_list(Type, List) ->
    [ decode(Type, Elem) || Elem <- List ].


%%
%%
%%
decode_atom(null) ->
    undefined;

decode_atom(Atom) when is_atom(Atom) ->
    Atom;

decode_atom(Atom) when is_list(Atom) ->
    erlang:list_to_existing_atom(Atom);

decode_atom(Atom) when is_binary(Atom) ->
    erlang:binary_to_existing_atom(Atom, utf8).


%%
%%
%%
decode_binary(null) ->
    undefined;

decode_binary(undefined) ->
    undefined;

decode_binary(Binary) when is_binary(Binary) ->
    Binary;

decode_binary(Binary) when is_integer(Binary) ->
    erlang:integer_to_binary(Binary).


%%
%%
%%
decode_integer(null) ->
    undefined;

decode_integer(undefined) ->
    undefined;

decode_integer(Integer) when is_integer(Integer) ->
    Integer;

decode_integer(Integer) when is_list(Integer) ->
    erlang:list_to_integer(Integer);

decode_integer(Integer) when is_binary(Integer) ->
    erlang:binary_to_integer(Integer).



%% =============================================================================
%%  Helpers for HAL support.
%% =============================================================================

%%
%%  Formats API URL.
%%
url(Path) ->
    ls1mcs_yaws:url(api, lists:map(fun to_string/1, Path)).


%%
%%  Used for constructing URLs.
%%
to_string(String) when is_atom(String) ->
    erlang:atom_to_list(String);

to_string(String) when is_list(String) ->
    String;

to_string(Integer) when is_integer(Integer) ->
    erlang:integer_to_list(Integer).


%%
%%  Produces HAL _links attribute.
%%
links(Links) ->
    {'_links', {Links}}.

%%
%%  Produces HAL link.
%%
link(Name, Url) ->
    {Name, {[
        {href, Url}
    ]}}.


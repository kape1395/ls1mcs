-module(ls1mcs_yaws_json).
-export([encode_list/1, encode/1, command_types/0]).
-include("ls1mcs.hrl").


%%
%%
%%
encode_list(List) ->
    [ encode(E) || E <- List ].


%%
%%
%%
encode(#command_type{desc = Desc, addr = Addr, port = Port, ack = Ack, params = Params}) ->
    {[
        {desc, Desc},
        {addr, Addr},
        {port, Port},
        {ack, Ack},
        {params, encode_list(Params)}
    ]};

encode(#cmd_param_spec{desc = Desc, type = Type, enum = Enum}) ->
    EnumEncoded = case Enum of
        undefined -> [];
        _ -> [ {enum, encode_list(Enum)} ]
    end,
    {[
        {desc, Desc},
        {type, Type}
    ] ++ EnumEncoded};

encode(#cmd_enum_spec{desc = Desc, value = Value}) ->
    {[
        {desc, Desc},
        {value, Value}
    ]}.


%%
%%
%%
command_types() ->
    [
        %%
        %%  ARM commands
        %%
        #command_type{addr = arm, port = ping, ack = true, desc = <<"Ping">>, params = [
        ]},
        #command_type{addr = arm, port = kill, ack = true, desc = <<"Terminate scheduled command">>, params = [
            #cmd_param_spec{type = integer, desc = <<"Command ref id">>}
        ]},
        #command_type{addr = arm, port = dlnk, ack = false, desc = <<"Downlink buffer fragment">>, params = [
            #cmd_param_spec{type = integer, desc = <<"Buffer">>, enum = [
                #cmd_enum_spec{desc = <<"Command Log">>,       value = 0},
                #cmd_enum_spec{desc = <<"Telemetry Archive">>, value = 1},
                #cmd_enum_spec{desc = <<"EPS Log">>,           value = 2},
                #cmd_enum_spec{desc = <<"GPS Binary Log">>,    value = 3},
                #cmd_enum_spec{desc = <<"GPS NMEA Log">>,      value = 4},
                #cmd_enum_spec{desc = <<"Helium Log">>,        value = 5}
            ]},
            #cmd_param_spec{type = integer, desc = <<"Frame from (inclusive)">>},
            #cmd_param_spec{type = integer, desc = <<"Frame till (exclusive)">>}
        ]},
        #command_type{addr = arm, port = rttm, ack = true, desc = <<"Downlink realtime telemetry">>, params = [
        ]},
        #command_type{addr = arm, port = jobp, ack = true, desc = <<"Set job period">>, params = [
            #cmd_param_spec{type = integer, desc = <<"Job">>, enum = [
                #cmd_enum_spec{desc = <<"Telemetry broadcast update">>,     value = 0},
                #cmd_enum_spec{desc = <<"Housekeeping tm collection">>,     value = 1},
                #cmd_enum_spec{desc = <<"Attitude telemetry collection">>,  value = 2},
                #cmd_enum_spec{desc = <<"GPS telemetry collection">>,       value = 3}
            ]},
            #cmd_param_spec{type = integer, desc = <<"Interval (secs)">>}
        ]}
        %% Multi-command should not be visible to the end user.

        %%
        %%  Arduino commands.
        %%
        #command_type{addr = arduino, port = take_photo, ack = true, desc = <<"Take a photo">>, params = [
        ]},
        #command_type{addr = arduino, port = photo_meta, ack = true, desc = <<"Downlink photo metadata">>, params = [
        ]},
        #command_type{addr = arduino, port = photo_data, ack = true, desc = <<"Downlink photo fragment">>, params = [
            #cmd_param_spec{type = integer, desc = <<"Frame from (inclusive)">>},
            #cmd_param_spec{type = integer, desc = <<"Frame till (exclusive)">>}
        ]}
    ].



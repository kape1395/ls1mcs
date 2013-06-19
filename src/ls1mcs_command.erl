-module(ls1mcs_command).
-export([command_addresses/0, user_cmd_specs/0]).
-include("ls1mcs.hrl").


%%
%%
%%
command_addresses() ->
    [
        #command_address{addr = arm,     desc = <<"ARM">>},
        #command_address{addr = arduino, desc = <<"Arduino">>},
        #command_address{addr = eps,     desc = <<"EPS">>},
        #command_address{addr = gps,     desc = <<"GPS">>},
        #command_address{addr = helium,  desc = <<"Helium">>}
    ].


%%
%%
%%
user_cmd_specs() ->
    [
        %%
        %%  ARM commands
        %%
        #user_cmd_spec{addr = arm, port = ping, ack = true, desc = <<"Ping">>, params = [
        ]},
        #user_cmd_spec{addr = arm, port = kill, ack = true, desc = <<"Terminate scheduled command">>, params = [
            #user_cmd_param{name = cref, type = integer, desc = <<"Command ref id">>}
        ]},
        #user_cmd_spec{addr = arm, port = downlink, desc = <<"Downlink buffer fragment">>, params = [
            #user_cmd_param{name = bufid, type = integer, desc = <<"Buffer">>, enum = [
                #user_cmd_enum{desc = <<"Command Log">>,       value = 0},
                #user_cmd_enum{desc = <<"Telemetry Archive">>, value = 1},
                #user_cmd_enum{desc = <<"EPS Log">>,           value = 2},
                #user_cmd_enum{desc = <<"GPS Binary Log">>,    value = 3},
                #user_cmd_enum{desc = <<"GPS NMEA Log">>,      value = 4},
                #user_cmd_enum{desc = <<"Helium Log">>,        value = 5}
            ]},
            #user_cmd_param{name = from, type = integer, desc = <<"Frame from (inclusive)">>},
            #user_cmd_param{name = till, type = integer, desc = <<"Frame till (exclusive)">>}
        ]},
        #user_cmd_spec{addr = arm, port = runtime_tm, ack = true, desc = <<"Downlink realtime telemetry">>, params = [
        ]},
        #user_cmd_spec{addr = arm, port = job_period, ack = true, desc = <<"Set job period">>, params = [
            #user_cmd_param{name = jobid, type = integer, desc = <<"Job">>, enum = [
                #user_cmd_enum{desc = <<"Telemetry broadcast update">>,     value = 0},
                #user_cmd_enum{desc = <<"Housekeeping tm collection">>,     value = 1},
                #user_cmd_enum{desc = <<"Attitude telemetry collection">>,  value = 2},
                #user_cmd_enum{desc = <<"GPS telemetry collection">>,       value = 3}
            ]},
            #user_cmd_param{name = interval, type = integer, desc = <<"Interval (secs)">>}
        ]}
        %% Multi-command should not be visible to the end user.

        %%
        %%  Arduino commands.
        %%
        #user_cmd_spec{addr = arduino, port = take_photo, ack = true, desc = <<"Take a photo">>, params = [
        ]},
        #user_cmd_spec{addr = arduino, port = photo_meta, ack = true, desc = <<"Downlink photo metadata">>, params = [
        ]},
        #user_cmd_spec{addr = arduino, port = photo_data, ack = true, desc = <<"Downlink photo fragment">>, params = [
            #user_cmd_param{name = from, type = integer, desc = <<"Frame from (inclusive)">>},
            #user_cmd_param{name = till, type = integer, desc = <<"Frame till (exclusive)">>}
        ]},

        %%
        %%  EPS Commands
        %%
        #user_cmd_spec{addr = eps, port = command, ack = true, desc = <<"Generic EPS Command">>, params = [
            #user_cmd_param{type = string, desc = <<"Command in hex">>}
        ]},

        %%
        %%  GPS Commands
        %%
        #user_cmd_spec{addr = gps, port = binary, ack = true, desc = <<"Generic GPS Binary Command">>, params = [
            #user_cmd_param{name = payload, type = string, desc = <<"Command in hex">>}
        ]},
        #user_cmd_spec{addr = gps, port = nmea, ack = true, desc = <<"Generic GPS NMEA Command">>, params = [
            #user_cmd_param{name = payload, type = string, desc = <<"Command">>}
        ]},

        %%
        %%  Helium-100 Commands
        %%
        #user_cmd_spec{addr = helium, port = command, ack = true, desc = <<"Generic Helium Command">>, params = [
            #user_cmd_param{name = payload, type = string, desc = <<"Command in hex">>}
        ]}
    ].



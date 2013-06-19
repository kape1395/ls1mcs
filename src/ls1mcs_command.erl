-module(ls1mcs_command).
-export([command_addresses/0, command_specs/0]).
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
command_specs() ->
    [
        %%
        %%  ARM commands
        %%
        #command_spec{addr = arm, port = ping, ack = true, desc = <<"Ping">>, params = [
        ]},
        #command_spec{addr = arm, port = kill, ack = true, desc = <<"Terminate scheduled command">>, params = [
            #cmd_param{name = cref, type = integer, desc = <<"Command ref id">>}
        ]},
        #command_spec{addr = arm, port = downlink, desc = <<"Downlink buffer fragment">>, params = [
            #cmd_param{name = bufid, type = integer, desc = <<"Buffer">>, enum = [
                #cmd_enum{desc = <<"Command Log">>,       value = 0},
                #cmd_enum{desc = <<"Telemetry Archive">>, value = 1},
                #cmd_enum{desc = <<"EPS Log">>,           value = 2},
                #cmd_enum{desc = <<"GPS Binary Log">>,    value = 3},
                #cmd_enum{desc = <<"GPS NMEA Log">>,      value = 4},
                #cmd_enum{desc = <<"Helium Log">>,        value = 5}
            ]},
            #cmd_param{name = from, type = integer, desc = <<"Frame from (inclusive)">>},
            #cmd_param{name = till, type = integer, desc = <<"Frame till (exclusive)">>}
        ]},
        #command_spec{addr = arm, port = runtime_tm, ack = true, desc = <<"Downlink realtime telemetry">>, params = [
        ]},
        #command_spec{addr = arm, port = job_period, ack = true, desc = <<"Set job period">>, params = [
            #cmd_param{name = jobid, type = integer, desc = <<"Job">>, enum = [
                #cmd_enum{desc = <<"Telemetry broadcast update">>,     value = 0},
                #cmd_enum{desc = <<"Housekeeping tm collection">>,     value = 1},
                #cmd_enum{desc = <<"Attitude telemetry collection">>,  value = 2},
                #cmd_enum{desc = <<"GPS telemetry collection">>,       value = 3}
            ]},
            #cmd_param{name = interval, type = integer, desc = <<"Interval (secs)">>}
        ]}
        %% Multi-command should not be visible to the end user.

        %%
        %%  Arduino commands.
        %%
        #command_spec{addr = arduino, port = take_photo, ack = true, desc = <<"Take a photo">>, params = [
        ]},
        #command_spec{addr = arduino, port = photo_meta, ack = true, desc = <<"Downlink photo metadata">>, params = [
        ]},
        #command_spec{addr = arduino, port = photo_data, ack = true, desc = <<"Downlink photo fragment">>, params = [
            #cmd_param{name = from, type = integer, desc = <<"Frame from (inclusive)">>},
            #cmd_param{name = till, type = integer, desc = <<"Frame till (exclusive)">>}
        ]},

        %%
        %%  EPS Commands
        %%
        #command_spec{addr = eps, port = command, ack = true, desc = <<"Generic EPS Command">>, params = [
            #cmd_param{type = string, desc = <<"Command in hex">>}
        ]},

        %%
        %%  GPS Commands
        %%
        #command_spec{addr = gps, port = binary, ack = true, desc = <<"Generic GPS Binary Command">>, params = [
            #cmd_param{name = payload, type = string, desc = <<"Command in hex">>}
        ]},
        #command_spec{addr = gps, port = nmea, ack = true, desc = <<"Generic GPS NMEA Command">>, params = [
            #cmd_param{name = payload, type = string, desc = <<"Command">>}
        ]},

        %%
        %%  Helium-100 Commands
        %%
        #command_spec{addr = helium, port = command, ack = true, desc = <<"Generic Helium Command">>, params = [
            #cmd_param{name = payload, type = string, desc = <<"Command in hex">>}
        ]}
    ].



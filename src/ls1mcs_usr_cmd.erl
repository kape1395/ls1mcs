%%
%%  User commands.
%%
-module(ls1mcs_usr_cmd).
-export([add/1, groups/0, specs/0]).
-export([send_sat_cmd/3]).
-export([sat_cmd_failed/2, sat_cmd_completed/2]).
-include("ls1mcs.hrl").



-callback sat_cmd_status(
        UsrCmdId :: term(),
        SatCmdId :: term(),
        Status :: (completed | failed)
    ) -> ok.



%%
%%  Call this to execute new user cmd.
%%
add(UserCmd) when is_record(UserCmd, user_cmd) ->
    {ok, UserCmdId} = ls1mcs_store:add_user_cmd(UserCmd),
    % TODO
    {ok, UserCmdId}.


%%
%%  Used by usr_cmd implementations to send sat cmd
%%  (and receive its notifications later).
%%
send_sat_cmd(UsrCmdMod, UsrCmdId, SatCmd) ->
    {ok, _FrameId} = ls1mcs_sat_link:send({usr_cmd_ref, UsrCmdMod, UsrCmdId}, SatCmd).


%%
%%  Invoked by sat_cmd.
%%
sat_cmd_failed(undefined, _SatCmdId) ->
    ok; % for frames, not initated by usr_cmd

sat_cmd_failed({usr_cmd_ref, UsrCmdMod, UsrCmdId}, SatCmdId) ->
    ok = UsrCmdMod:sat_cmd_status(UsrCmdId, SatCmdId, failed),
    ok.


%%
%%  Invoked by sat_cmd.
%%
sat_cmd_completed(undefined, _SatCmdId) ->
    ok; % for frames, not initated by usr_cmd

sat_cmd_completed({usr_cmd_ref, UsrCmdMod, UsrCmdId}, SatCmdId) ->
    ok = UsrCmdMod:sat_cmd_status(UsrCmdId, SatCmdId, completed),
    ok.


%%
%%  Returns a list of available user command groups.
%%
groups() ->
    [
        #user_cmd_group{name = arm,     desc = <<"ARM">>},
        #user_cmd_group{name = arduino, desc = <<"Arduino">>},
        #user_cmd_group{name = eps,     desc = <<"EPS">>},
        #user_cmd_group{name = gps,     desc = <<"GPS">>},
        #user_cmd_group{name = helium,  desc = <<"Helium">>}
    ].


%%
%%  Returns a list of available user command specs.
%%
specs() ->
    [
        %%
        %%  ARM commands
        %%
        #user_cmd_spec{group = arm, name = ping, desc = <<"Ping">>, params = [
        ]},
        #user_cmd_spec{group = arm, name = kill, desc = <<"Terminate scheduled command">>, params = [
            #user_cmd_param{name = cref, type = integer, desc = <<"Command ref id">>}
        ]},
        #user_cmd_spec{group = arm, name = downlink, desc = <<"Downlink buffer fragment">>, params = [
            #user_cmd_param{name = bufid, type = integer, desc = <<"Buffer">>, opts = [
                #user_cmd_opts{desc = <<"Command Log">>,       value = 0},
                #user_cmd_opts{desc = <<"Telemetry Archive">>, value = 1},
                #user_cmd_opts{desc = <<"EPS Log">>,           value = 2},
                #user_cmd_opts{desc = <<"GPS Binary Log">>,    value = 3},
                #user_cmd_opts{desc = <<"GPS NMEA Log">>,      value = 4},
                #user_cmd_opts{desc = <<"Helium Log">>,        value = 5}
            ]},
            #user_cmd_param{name = from, type = integer, desc = <<"Frame from (inclusive)">>},
            #user_cmd_param{name = till, type = integer, desc = <<"Frame till (exclusive)">>}
        ]},
        #user_cmd_spec{group = arm, name = runtime_tm, desc = <<"Downlink realtime telemetry">>, params = [
        ]},
        #user_cmd_spec{group = arm, name = job_period, desc = <<"Set job period">>, params = [
            #user_cmd_param{name = jobid, type = integer, desc = <<"Job">>, opts = [
                #user_cmd_opts{desc = <<"Telemetry broadcast update">>,     value = 0},
                #user_cmd_opts{desc = <<"Housekeeping tm collection">>,     value = 1},
                #user_cmd_opts{desc = <<"Attitude telemetry collection">>,  value = 2},
                #user_cmd_opts{desc = <<"GPS telemetry collection">>,       value = 3}
            ]},
            #user_cmd_param{name = interval, type = integer, desc = <<"Interval (secs)">>}
        ]},
        %% Multi-command should not be visible to the end user.

        %%
        %%  Arduino commands.
        %%
        #user_cmd_spec{group = arduino, name = take_photo, desc = <<"Take a photo">>, params = [
            #user_cmd_param{name = resid, type = integer, desc = <<"Resolution">>, opts = [
                #user_cmd_opts{desc = <<"Resolution 1">>, value = 0},
                #user_cmd_opts{desc = <<"Resolution 2">>, value = 1},
                #user_cmd_opts{desc = <<"Resolution 3">>, value = 2}
            ]}
        ]},
        #user_cmd_spec{group = arduino, name = photo_meta, desc = <<"Downlink photo metadata">>, params = [
        ]},
        #user_cmd_spec{group = arduino, name = photo_data, desc = <<"Downlink photo fragment">>, params = [
            #user_cmd_param{name = from, type = integer, desc = <<"Frame from (inclusive)">>},
            #user_cmd_param{name = till, type = integer, desc = <<"Frame till (exclusive)">>}
        ]},

        %%
        %%  EPS Commands
        %%
        #user_cmd_spec{group = eps, name = eps_command, desc = <<"Generic EPS Command">>, params = [
            #user_cmd_param{type = string, desc = <<"Command in hex">>}
        ]},

        %%
        %%  GPS Commands
        %%
        #user_cmd_spec{group = gps, name = gps_binary_cmd, desc = <<"Generic GPS Binary Command">>, params = [
            #user_cmd_param{name = payload, type = string, desc = <<"Command in hex">>}
        ]},
        #user_cmd_spec{group = gps, name = gps_nmea_cmd, desc = <<"Generic GPS NMEA Command">>, params = [
            #user_cmd_param{name = payload, type = string, desc = <<"Command">>}
        ]},

        %%
        %%  Helium-100 Commands
        %%
        #user_cmd_spec{group = helium, name = he_command, desc = <<"Generic Helium Command">>, params = [
            #user_cmd_param{name = payload, type = string, desc = <<"Command in hex">>}
        ]}
    ].



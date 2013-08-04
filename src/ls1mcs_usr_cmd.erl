%%
%%  User commands.
%%  See issue/1 for the entry point.
%%
-module(ls1mcs_usr_cmd).
-compile([{parse_transform, lager_transform}]).
-export([start_link/1, issue/1, groups/0, specs/0]).
-export([send_sat_cmd/3]).
-export([sat_cmd_failed/2, sat_cmd_completed/2]).
-include("ls1mcs.hrl").


%% =============================================================================
%%  Callback definitions and functions for CB modules.
%% =============================================================================

%%
%%
%%
-callback sat_cmd_status(
        UsrCmdId :: term(),
        SatCmdId :: term(),
        Status :: (completed | failed)
    ) -> ok.


%%
%%  Used by usr_cmd implementations to send sat cmd
%%  (and receive its notifications later).
%%
send_sat_cmd(UsrCmdMod, UsrCmdId, SatCmd) ->
    {ok, _SatCmdId} = ls1mcs_sat_link:send(SatCmd, {usr_cmd_ref, UsrCmdMod, UsrCmdId}).



%% =============================================================================
%%  Public API.
%% =============================================================================

%%
%%  Starts the user command by invoking its implementation "start_link" funkcion.
%%
start_link({M, F, A}) ->
    erlang:apply(M, F, A).


%%
%%  Call this to issue new user cmd.
%%
issue(UsrCmd = #usr_cmd{spec = SpecName}) ->
    {ok, UsrCmdId} = ls1mcs_store:add_usr_cmd(UsrCmd),
    UsrCmdWithId = UsrCmd#usr_cmd{id = UsrCmdId},
    lager:debug("ls1mcs_usr_cmd: Issuing usr cmd: ~p", [UsrCmdWithId]),
    case lists:keyfind(SpecName, #usr_cmd_spec.name, specs()) of
        Spec = #usr_cmd_spec{impl = {M, F, A}} ->
            {ok, _Pid} = ls1mcs_usr_cmd_sup:add({M, F, [UsrCmdWithId, Spec | A]}),
            {ok, UsrCmdId};
        false ->
            {error, spec_not_found}
    end.


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
        #usr_cmd_group{name = arm,     desc = <<"ARM">>},
        #usr_cmd_group{name = arduino, desc = <<"Arduino">>},
        #usr_cmd_group{name = eps,     desc = <<"EPS">>},
        %#usr_cmd_group{name = gps,     desc = <<"GPS">>},
        #usr_cmd_group{name = helium,  desc = <<"Helium">>}
    ].


%%
%%  Returns a list of available user command specs.
%%
specs() ->
    Simple = {ls1mcs_usr_cmd_simple, start_link, []},
    [
        %%
        %%  ARM commands
        %%
        #usr_cmd_spec{group = arm, name = ping, desc = <<"Ping">>, impl = Simple, params = [
        ]},
        #usr_cmd_spec{group = arm, name = kill, desc = <<"Terminate scheduled command">>, impl = Simple, params = [
            #usr_cmd_param{name = cref, type = integer, desc = <<"Command ref id">>}
        ]},
        #usr_cmd_spec{group = arm, name = downlink, desc = <<"Downlink buffer fragment">>, impl = Simple, params = [
            #usr_cmd_param{name = bufid, type = integer, desc = <<"Buffer">>, opts = [
                #usr_cmd_opts{desc = <<"Command Log">>,       value = 0},
                #usr_cmd_opts{desc = <<"Telemetry Archive">>, value = 1},
                #usr_cmd_opts{desc = <<"EPS Log">>,           value = 2},
                #usr_cmd_opts{desc = <<"GPS Binary Log">>,    value = 3},
                #usr_cmd_opts{desc = <<"GPS NMEA Log">>,      value = 4},
                #usr_cmd_opts{desc = <<"Helium Log">>,        value = 5}
            ]},
            #usr_cmd_param{name = blksz, type = integer, desc = <<"Block size">>},
            #usr_cmd_param{name = from, type = integer, desc = <<"Block from (inclusive)">>},
            #usr_cmd_param{name = till, type = integer, desc = <<"Block till (exclusive)">>}
        ]},
        #usr_cmd_spec{group = arm, name = runtime_tm, desc = <<"Downlink realtime telemetry">>, impl = Simple, params = [
        ]},
        #usr_cmd_spec{group = arm, name = job_period, desc = <<"Set job period">>, impl = Simple, params = [
            #usr_cmd_param{name = jobid, type = integer, desc = <<"Job">>, opts = [
                #usr_cmd_opts{desc = <<"Telemetry broadcast update">>,     value = 0},
                #usr_cmd_opts{desc = <<"Housekeeping tm collection">>,     value = 1},
                #usr_cmd_opts{desc = <<"Attitude telemetry collection">>,  value = 2},
                #usr_cmd_opts{desc = <<"GPS telemetry collection">>,       value = 3}
            ]},
            #usr_cmd_param{name = interval, type = integer, desc = <<"Interval (secs)">>}
        ]},
        %% Multi-command should not be visible to the end user.

        %%
        %%  Arduino commands.
        %%
        #usr_cmd_spec{group = arduino, name = take_photo, desc = <<"Take a photo">>, impl = Simple, params = [
            #usr_cmd_param{name = resid, type = integer, desc = <<"Resolution">>, opts = [
                #usr_cmd_opts{desc = <<"Resolution 1">>, value = 0},
                #usr_cmd_opts{desc = <<"Resolution 2">>, value = 1},
                #usr_cmd_opts{desc = <<"Resolution 3">>, value = 2}
            ]},
            #usr_cmd_param{name = delay, type = integer, desc = <<"Delay (s)">>}
        ]},
        #usr_cmd_spec{group = arduino, name = photo_meta, desc = <<"Downlink photo metadata">>, impl = Simple, params = [
        ]},
        #usr_cmd_spec{group = arduino, name = photo_data, desc = <<"Downlink photo fragment">>, impl = Simple, params = [
            #usr_cmd_param{name = blksz, type = integer, desc = <<"Block size">>},
            #usr_cmd_param{name = from, type = integer, desc = <<"Block from (inclusive)">>},
            #usr_cmd_param{name = till, type = integer, desc = <<"Block till (exclusive)">>}
        ]},
        #usr_cmd_spec{group = arduino, name = dlnk_photo, desc = <<"Downlink photo">>,
            impl = {ls1mcs_usr_cmd_photo, start_link, []},
            params = []
        },

        %%
        %%  EPS Commands
        %%
        #usr_cmd_spec{group = eps, name = eps_command, desc = <<"Generic EPS Command">>, params = [
            #usr_cmd_param{type = string, desc = <<"Command in hex">>}
        ]},

        %%
        %%  GPS Commands
        %%
        %#usr_cmd_spec{group = gps, name = gps_binary_cmd, desc = <<"Generic GPS Binary Command">>, params = [
        %    #usr_cmd_param{name = payload, type = string, desc = <<"Command in hex">>}
        %]},
        %#usr_cmd_spec{group = gps, name = gps_nmea_cmd, desc = <<"Generic GPS NMEA Command">>, params = [
        %    #usr_cmd_param{name = payload, type = string, desc = <<"Command">>}
        %]},

        %%
        %%  Helium-100 Commands
        %%
        #usr_cmd_spec{group = helium, name = he_command, desc = <<"Generic Helium Command">>, params = [
            #usr_cmd_param{name = payload, type = string, desc = <<"Command in hex">>}
        ]}
    ].

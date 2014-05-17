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
%%  User commands.
%%  See issue/1 for the entry point.
%%
-module(ls1mcs_usr_cmd).
-compile([{parse_transform, lager_transform}]).
-export([start_link/1, issue/1, groups/0, specs/0, arg_val/2]).
-export([send_sat_cmd/3, set_status/2]).
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
    {ok, _SatCmdId} = ls1mcs_sat_cmd:issue(
        SatCmd#sat_cmd{usr_cmd_id = UsrCmdId},
        {usr_cmd_ref, UsrCmdMod, UsrCmdId}
    ).


%%
%%  Updates user command status.
%%
set_status(UsrCmdId, UsrCmdStatus) ->
    ok = ls1mcs_store:set_usr_cmd_status(UsrCmdId, UsrCmdStatus).



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
    UsrCmdNormalized = UsrCmd#usr_cmd{status = issued},
    {ok, UsrCmdId} = ls1mcs_store:add_usr_cmd(UsrCmdNormalized),
    UsrCmdWithId = UsrCmdNormalized#usr_cmd{id = UsrCmdId},
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
        #usr_cmd_group{name = helium,  desc = <<"Helium">>},
        #usr_cmd_group{name = misc,    desc = <<"Misc">>}
    ].


%%
%%  Returns a list of available user command specs.
%%
specs() ->
    Simple = {ls1mcs_usr_cmd_simple, start_link, []},
    EpsChannels = [
        #usr_cmd_opts{desc = <<"0 - 5V1 - Antenna deploym.">>,      value = 0},
        #usr_cmd_opts{desc = <<"1 - 5V2 - Helium-100 TX">>,         value = 1},
        #usr_cmd_opts{desc = <<"2 - 5V3 - FM repeater">>,           value = 2},
        #usr_cmd_opts{desc = <<"3 - 3.3V1 - Arduino + Beacon">>,    value = 3},
        #usr_cmd_opts{desc = <<"4 - 3.3V2 - ARM">>,                 value = 4},
        #usr_cmd_opts{desc = <<"5 - 3.3V3 - Helium-100 RX">>,       value = 5}
    ],
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
                #usr_cmd_opts{desc = <<"Operation Log">>,     value = 1},
                #usr_cmd_opts{desc = <<"Hk. data archive">>,  value = 2},
                #usr_cmd_opts{desc = <<"Att. data archive">>, value = 3}
            ]},
            #usr_cmd_param{name = blksz, type = integer, desc = <<"Block size">>},
            #usr_cmd_param{name = from, type = integer, desc = <<"Block from (inclusive)">>},
            #usr_cmd_param{name = till, type = integer, desc = <<"Block till (exclusive)">>}
        ]},
        #usr_cmd_spec{group = arm, name = runtime_tm, desc = <<"Downlink realtime telemetry">>, impl = Simple, params = [
        ]},
        #usr_cmd_spec{group = arm, name = job_period, desc = <<"Set job period">>, impl = Simple, params = [
            #usr_cmd_param{name = jobid, type = integer, desc = <<"Job">>, opts = [
                #usr_cmd_opts{desc = <<"Telemetry broadcast">>,            value = 0},
                #usr_cmd_opts{desc = <<"Housekeeping tm collection">>,     value = 1},
                #usr_cmd_opts{desc = <<"Attitude telemetry collection">>,  value = 2}
            ]},
            #usr_cmd_param{name = interval, type = integer, desc = <<"Interval (secs)">>}
        ]},
        #usr_cmd_spec{group = arm, name = pwr_allow_nm, desc = <<"Allow PWR nominal">>, impl = Simple, params = [
            #usr_cmd_param{name = allow, type = integer, desc = <<"Job">>, opts = [
                #usr_cmd_opts{desc = <<"Allow">>,       value = 1},
                #usr_cmd_opts{desc = <<"Disallow">>,    value = 0}
            ]}
        ]},
        #usr_cmd_spec{group = arm, name = pwr_state, desc = <<"Set PWR state">>, impl = Simple, params = [
            #usr_cmd_param{name = mode, type = integer, desc = <<"Mode">>, opts = [
                #usr_cmd_opts{desc = <<"Auto">>,       value = 0},
                #usr_cmd_opts{desc = <<"Safe">>,       value = 1},
                #usr_cmd_opts{desc = <<"Nominal">>,    value = 2}
            ]}
        ]},
        #usr_cmd_spec{group = arm, name = term_sci_mode, desc = <<"Terminate SCI mode">>, impl = Simple, params = [
        ]},
        #usr_cmd_spec{group = arm, name = start_fmrep, desc = <<"Start FM Repeater">>, impl = Simple, params = [
            #usr_cmd_param{name = delay,    type = integer, desc = <<"Delay">>},
            #usr_cmd_param{name = duration, type = integer, desc = <<"Duration">>}
        ]},
        #usr_cmd_spec{group = arm, name = sd_format, desc = <<"Format ARM SD Card">>, impl = Simple, params = [
        ]},
        #usr_cmd_spec{group = arm, name = set_started, desc = <<"Set SAT started">>, impl = Simple, params = [
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
        #usr_cmd_spec{group = arduino, name = beacon_st, desc = <<"Beacon">>, impl = Simple, params = [
            #usr_cmd_param{name = state, type = integer, desc = <<"New state">>, opts = [
                #usr_cmd_opts{desc = <<"On">>, value = 1},
                #usr_cmd_opts{desc = <<"Off">>, value = 0}
            ]}
        ]},

        %%
        %%  EPS-related commands
        %%
        #usr_cmd_spec{group = eps, name = eps_ch_status, desc = <<"Set EPS channel status">>, impl = Simple, params = [
            #usr_cmd_param{name = channel, type = integer, desc = <<"Channel">>, opts = EpsChannels},
            #usr_cmd_param{name = status, type = integer, desc = <<"Status">>, opts = [
                #usr_cmd_opts{desc = <<"On">>, value = 1},
                #usr_cmd_opts{desc = <<"Off">>, value = 0}
            ]}
        ]},
        #usr_cmd_spec{group = eps, name = eps_ch_onoff, desc = <<"Set EPS channel status">>, impl = Simple, params = [
            #usr_cmd_param{name = channel,  type = integer, desc = <<"Channel">>, opts = EpsChannels},
            #usr_cmd_param{name = delay,    type = integer, desc = <<"Start delay">>},
            #usr_cmd_param{name = duration, type = integer, desc = <<"Duration">>}
        ]},
        #usr_cmd_spec{group = eps, name = hrd_reset, desc = <<"SAT Hard Reset">>, impl = Simple, params = [
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
        #usr_cmd_spec{group = helium, name = he_restore, desc = <<"Restore Helium Config">>, impl = Simple, params = [
        ]},
        #usr_cmd_spec{group = helium, name = he_tx_prw, desc = <<"Set Helium TX Power Level">>, impl = Simple, params = [
            #usr_cmd_param{name = level, type = integer, desc = <<"Amplification level (00-FF)">>}
        ]},

        %%
        %%  Misc commands.
        %%
        #usr_cmd_spec{group = misc, name = shed_ping, desc = <<"Scheduled ping">>,
            impl = {ls1mcs_usr_cmd_scheduled, start_link, []},
            params = [
                #usr_cmd_param{name = from,  type = string,  desc = <<"From date (yyyy-MM-dd HH:mm:ss, UTC)">>},
                #usr_cmd_param{name = till,  type = string,  desc = <<"Till date (yyyy-MM-dd HH:mm:ss, UTC)">>},
                #usr_cmd_param{name = retry, type = integer, desc = <<"Retry interval (sec)">>}
            ]
        }
    ].


%%
%%  Returns user command argument as an integer.
%%
arg_val(Name, Args) ->
    #usr_cmd_arg{name = Name, value = Value} = lists:keyfind(Name, #usr_cmd_arg.name, Args),
    erlang:binary_to_integer(Value).


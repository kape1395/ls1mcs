-ifndef(LS1MCS_HRL).
-define(LS1MCS_HRL, 1).
-include("ls1p.hrl").

-type sat_cmd_id() :: integer().
-type usr_cmd_id() :: integer().
-type usr_cmd_ref() :: {usr_cmd_ref, module(), usr_cmd_id()}.

-type cref() :: ls1p_cref().
-type timestamp() :: os:timestamp().


%% =============================================================================
%%  Commands.
%% =============================================================================

%%
%%  Options for user command parameters.
%%  Enum is a valid name for this entity.
%%  The latter is not used because enum is reserved name in some languages.
%%
-record(usr_cmd_opts, {
    desc        :: binary(),
    value       :: integer() | float() | atom()
}).

%%
%%  Describes parameter for a user command.
%%
-record(usr_cmd_param, {
    name        :: atom(),
    desc        :: binary(),
    type        :: (integer | float | string | opts),
    opts        :: [#usr_cmd_opts{}]
}).

%%
%%  Used for grouping user commands.
%%
-record(usr_cmd_group, {
    desc        :: binary(),
    name        :: atom()
}).

%%
%%  Describes user command.
%%
-record(usr_cmd_spec, {
    desc            :: binary(),
    name            :: atom(),
    group           :: atom(),                      %% Group name, see #usr_cmd_group{}.
    params = []     :: [#usr_cmd_param{}],
    impl            :: {module(), atom(), list()}   %% MFA; #usr_cmd{} is added as a first param.
}).

%%
%%  Command arguments, instances of #usr_cmd_param{}.
%%
-record(usr_cmd_arg, {
    name        :: atom(),
    value       :: binary()
}).

%%
%%  Single command issued by a user.
%%  These commands are instances of #usr_cmd_spec{}.
%%
%%  TODO: Add plan.
%%
-record(usr_cmd, {
    id          :: usr_cmd_id(),        % Auto-generated id.
    spec        :: atom(),              % Spec name.
    args        :: [#usr_cmd_arg{}],
    immediate   :: boolean(),
    approved    :: timestamp(),
    issued      :: timestamp(),
   %confirmed   :: {User :: binary(), Time :: timestamp()},  % TODO: User confirmed the success of the command manually.
    status      :: atom()               % TODO: List of values?
}).


%%
%%  TODO: Review
%%  Represents single LS1P command.
%%  It can be composite or atomic.
%%
-record(sat_cmd, {
    id          :: sat_cmd_id(),
    usr_cmd_id  :: usr_cmd_id(),
    cmd_frame   :: #ls1p_cmd_frame{},
    exp_dfc = 0 :: integer(),           %% Expected data frame count.
    acked       :: timestamp(),
    executed    :: timestamp(),
    dat_recv    :: timestamp(),         %% time of the first data frame received.
    eof_recv    :: timestamp(),         %% time of the last data frame received.
    log_recv    :: timestamp()          %% time of the last cmd_log entry received.
}).

%%
%%  TODO: Review
%%  Single command frame, sent over the radio link.
%%  These frames are generated based on issued #usr_cmd{}s.
%%  The frame can have multiple #sat_cmd{}s, in the case of multi-command.
%%
-record(cmd_frame, {
    id          :: integer(),       %%
    sat_cmds    :: [integer()],     %%
    sent        :: timestamp(),     %%
    status      :: atom()           %% status of the command
}).


%% =============================================================================
%%  SAT related structures.
%% =============================================================================

%%
%%  See GPredict user manual
%%
-record(predicted_pass, {
    aos,
    tca,
    los,
    duration,
    max_el,
    aos_az,
    max_el_az,
    los_az,
    orbit,
    visible,        % Visibility
    daylight,       % Visibility
    eclipsed        % Visibility
}).



%% =============================================================================
%%  Telemetry.
%% =============================================================================

-type uint8() :: non_neg_integer().
-type sint16() :: integer().
-type uint16() :: non_neg_integer().
-type uint32() :: non_neg_integer().

%%
%%  Helium-100 (transceiver) data.
%%
-record(tm_he, {
    op_counter          :: uint16(),
    msp430_temp         :: sint16(),
    time_count_1        :: uint8(),
    time_count_2        :: uint8(),
    time_count_3        :: uint8(),
    rssi                :: uint8(),
    bytes_received      :: uint32(),
    bytes_transmitted   :: uint32()
}).

%%
%%  Magnetometer data.
%%
-record(tm_mag, {
    x       :: float(),
    y       :: float(),
    z       :: float()
}).


%%
%%  Accelerometer data.
%%
-record(tm_accel, {
    x       :: float(),
    y       :: float(),
    z       :: float()
}).

%%
%%  Gyroscope data.
%%
-record(tm_gyro, {
    x       :: float(),
    y       :: float(),
    z       :: float(),
    temp    :: float()
}).


%%
%%  P31U (power unit) data.
%%
-record(tm_eps, {
    pv_1                :: uint16(),
    pv_2                :: uint16(),
    pv_3                :: uint16(),
    pc                  :: uint16(),
    bv                  :: uint16(),
    sc                  :: uint16(),
    temp_BC1            :: sint16(),
    temp_BC2            :: sint16(),
    temp_BC3            :: sint16(),
    temp_OB             :: sint16(),
    batt_temp_1         :: sint16(),
    batt_temp_2         :: sint16(),
    latchup_50V1        :: uint16(),
    latchup_50V2        :: uint16(),
    latchup_50V3        :: uint16(),
    latchup_33V1        :: uint16(),
    latchup_33V2        :: uint16(),
    latchup_33V3        :: uint16(),
    reset               :: uint8(),
    bootcount           :: uint16(),
    sw_errors           :: uint16(),
    ppt_mode            :: uint8(),
    channel_status_QH   :: boolean(),
    channel_status_QS   :: boolean(),
    channel_status_50V1 :: boolean(),
    channel_status_50V2 :: boolean(),
    channel_status_50V3 :: boolean(),
    channel_status_33V1 :: boolean(),
    channel_status_33V2 :: boolean(),
    channel_status_33V3 :: boolean()
}).


%%
%%  Housekeeping data.
%%
-record(tm_hk, {
    pwr_mode    :: integer(),
    sat_mode    :: integer(),
    eps         :: #tm_eps{},
    he          :: #tm_he{}
}).

%%
%%  Attitude data.
%%
-record(tm_att, {
    s_HMC5883L_mag   :: #tm_mag{},
    s_MPU6000A_accel :: #tm_accel{},
    s_MPU6000A_gyro  :: #tm_gyro{},
    s_MPU9150A_accel :: #tm_accel{},
    s_MPU9150A_gyro  :: #tm_gyro{},
    s_AK8975_mag     :: #tm_mag{},
    s_L3GD20_gyro    :: #tm_gyro{}
}).

%%
%%  Telemetry data.
%%
-record(tm, {
    time    :: float(),     %% SAT time.
    hk      :: #tm_hk{},
    att     :: [#tm_att{}]
}).


-endif.


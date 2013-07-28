-ifndef(LS1MCS_HRL).
-define(LS1MCS_HRL, 1).
-include("ls1p.hrl").

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
-record(user_cmd_opts, {
    desc        :: binary(),
    value       :: integer() | float() | atom()
}).

%%
%%  Describes parameter for a user command.
%%
-record(user_cmd_param, {
    name        :: atom(),
    desc        :: binary(),
    type        :: (integer | float | string | opts),
    opts        :: [#user_cmd_opts{}]
}).

%%
%%  Used for grouping user commands.
%%
-record(user_cmd_group, {
    desc        :: binary(),
    name        :: atom()
}).

%%
%%  Describes user command.
%%
-record(user_cmd_spec, {
    desc            :: binary(),
    name            :: atom(),
    group           :: atom(),      %%  Group name, see #user_cmd_group{}.
    params = []     :: [#user_cmd_param{}]
}).

%%
%%  Command arguments, instances of #user_cmd_param{}.
%%
-record(user_cmd_arg, {
    name        :: atom(),
    value       :: integer() | float() | binary()
}).

%%
%%  Single command issued by a user.
%%  These commands are instances of #user_cmd_spec{}.
%%
-record(user_cmd, {
    id          :: integer(),
    spec        :: atom(),
    params      :: [#user_cmd_arg{}],
    immediate   :: boolean(),
    approved    :: timestamp(),
    issued      :: timestamp(),
    status      :: atom()               % TODO: List of values?
}).


%%
%%  Represents single LS1P command.
%%  It can be composite or atomic.
%%
-record(sat_cmd, {
    id          :: integer(),
    cmd_frame   :: integer(),
    ls1p_frame  :: #ls1p_cmd_frame{},   %% CRef is in this structure.
    acked       :: timestamp(),
    executed    :: timestamp(),
    dat_recv    :: timestamp(),         %% time of the first data frame received.
    eof_recv    :: timestamp(),         %% time of the last data frame received.
    log_recv    :: timestamp()          %% time of the last cmd_log entry received.
}).

%%
%%  Single command frame, sent over the radio link.
%%  These frames are generated based on issued #user_cmd{}s.
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
%%  Transceiver data.
%%  He-100, 16 bytes
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
%%  HMC5883L, 6 bytes
%%
-record(tm_mag, {
    bx      :: uint16(),
    by      :: uint16(),
    bz      :: uint16()
}).

%%
%%  MPU-6000A, 14 bytes
%%
-record(tm_mpu, {
    gx      :: uint16(),
    gy      :: uint16(),
    gz      :: uint16(),
    ax      :: uint16(),
    ay      :: uint16(),
    az      :: uint16(),
    temp    :: uint16()
}).

%%
%%  L3GD20, 7 bytes
%%
-record(tm_gyro, {
    wx      :: uint16(),
    wy      :: uint16(),
    wz      :: uint16(),
    temp    :: uint8()
}).

%%
%%  P31U, 43 bytes
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
%%  Total: 34 bytes
%%
-record(tm_att, {
    mag     :: #tm_mag{},
    mpu     :: #tm_mpu{},
    gyro_1  :: #tm_gyro{},
    gyro_2  :: #tm_gyro{}
}).

%%
%%  Total: 233 bytes
%%
-record(tm, {
    time    :: uint32(),    %% Centi-seconds (1s / 100).
    eps     :: #tm_eps{},
    he      :: #tm_he{},
    att     :: [#tm_att{}]  %% x5
}).


-endif.


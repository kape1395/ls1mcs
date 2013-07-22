-ifndef(LS1MCS_HRL).
-define(LS1MCS_HRL, 1).
-include("ls1p.hrl").

-type cref() :: ls1p_cref().
-type timestamp() :: os:timestamp().


%% =============================================================================
%%  AX.25 Structures.
%% =============================================================================

-record(ax25_addr, {
    call    :: list(),
    ssid    :: integer()
}).
-record(ax25_frame, {
    dst     :: #ax25_addr{},
    src     :: #ax25_addr{},
    data    :: binary()
}).



%% =============================================================================
%%  Command specifications.
%% =============================================================================


-record(user_cmd_enum, {
    desc        :: binary(),
    value       :: integer() | float() | atom()
}).

-record(user_cmd_param, {
    name        :: atom(),
    desc        :: binary(),
    type        :: (integer | float | string | enum),
    enum        :: [#user_cmd_enum{}]
}).

-record(user_cmd_spec, {
    desc            :: binary(),
    addr            :: atom(),
    port            :: atom(),
    ack = false     :: boolean(),   %%  Default value for the ack field.
    comp = false    :: boolean(),   %%  True, if the command is translated to several commands before sending to SAT.
    params = []     :: [#user_cmd_param{}]
}).

-record(command_address, {
    desc        :: binary(),
    addr        :: atom()
}).


%% =============================================================================
%%  Commands.
%% =============================================================================

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

%%
%%  Command arguments, instances of cmd_param.
%%
-record(user_cmd_arg, {
    name        :: atom(),
    value       :: integer() | float() | binary()
}).

%%
%%  Single command issued by a user.
%%  These commands are instances of command_spec.
%%
-record(user_cmd, {
    id          :: integer(),
    addr        :: atom(),
    port        :: atom(),
    ack         :: boolean(),
    delay       :: integer(),
    params      :: [#user_cmd_arg{}],
    immediate   :: boolean(),
    approved    :: timestamp(),     %% auto | true | false ?
    issued      :: timestamp(),
    cmd_frames  :: [#cmd_frame{}],
    status      :: atom()
}).

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

-type uint8() :: integer().
-type uint16() :: integer().
-type uint32() :: integer().

-record(tm_he, {
}).
-record(tm_mag, {
}).
-record(tm_mpu, {
}).
-record(tm_gyro, {
}).

-record(tm_eps, {
    pv      :: [uint16()],                      %% Photo-voltaic input voltage [mV] * 3
    pc      :: uint16(),                        %% Total photo current [mA]
    bv      :: uint16(),                        %% Battery voltage [mV]
    sc      :: uint16()                         %% Total system current [mA]
}).

-record(tm_att, {               %% Total: 34 bytes
    mag     :: #tm_mag{},       %% HMC5883L,    6  bytes
    mpu     :: #tm_mpu{},       %% MPU-6000A,   14 bytes
    gyro    :: [#tm_gyro{}]     %% L3GD20,      14 bytes = 7 bytes * 2
}).

-record(telemetry, {            %% Total: 233 bytes
    time    :: uint32(),        %% ---,         4 bytes
    eps     :: #tm_eps{},       %% P31U,        43 bytes
    he      :: #tm_he{},        %% He-100,      16 bytes
    att     :: [#tm_att{}]      %% ---,         170 bytes = 34 bytes * 5
}).




-endif.


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


-record(cmd_enum_spec, {
    desc        :: binary(),
    value       :: integer() | float() | atom()
}).

-record(cmd_param_spec, {
    name        :: atom(),
    desc        :: binary(),
    type        :: (integer | float | string | enum),
    enum        :: [#cmd_enum_spec{}]
}).

-record(command_spec, {
    desc            :: binary(),
    addr            :: atom(),
    port            :: atom(),
    ack = false     :: boolean(),   %%  Default value for the ack field.
    comp = false    :: boolean(),   %%  True, if the command is translated to several commands before sending to SAT.
    params = []     :: [#cmd_param_spec{}]
}).

-record(command_address, {
    desc        :: binary(),
    addr        :: atom()
}).


%% =============================================================================
%%  Commands.
%% =============================================================================

%%
%%  Single command issued by a user.
%%  These commands are instances of
%%
-record(cmd_frame, {
    frame       :: #ls1p_cmd_frame{},
    sent        :: timestamp(),
    acked       :: timestamp(),
    executed    :: timestamp(),
    dat_recv    :: timestamp(),     %% time of the first data frame received.
    eof_recv    :: timestamp(),     %% time of the last data frame received.
    log_recv    :: timestamp(),     %% time of the last cmd_log entry received.
    status      :: atom()           %% status of the command
}).

-record(cmd_param, {
    name        :: atom(),
    value       :: integer() | float() | binary()
}).

-record(command, {
    cref        :: cref(),
    addr        :: atom(),
    port        :: atom(),
    ack         :: boolean(),
    delay       :: integer(),
    params      :: [#cmd_param{}],
    immediate   :: boolean(),
    approved    :: timestamp(),     %% auto | true | false ?
    issued      :: timestamp(),
    cmd_frames  :: [#cmd_frame{}],
    status      :: atom()
}).



-endif.


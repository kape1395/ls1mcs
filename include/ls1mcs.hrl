
%%
%%  AX.25 Structures.
%%

-record(ax25_addr, {
    call    :: list(),
    ssid    :: integer()
}).
-record(ax25_frame, {
    dst     :: #ax25_addr{},
    src     :: #ax25_addr{},
    data    :: binary()
}).



%%
%%  Command list.
%%

-record(cmd_enum_spec, {
    desc        :: binary(),
    value       :: integer() | float() | atom()
}).
-record(cmd_param_spec, {
    desc        :: binary(),
    type        :: (integer | float | string | enum),
    enum        :: [#cmd_enum_spec{}]
}).
-record(command_type, {
    desc        :: binary(),
    addr        :: atom(),
    port        :: atom(),
    ack         :: boolean(),           % Default value for the ack field.
    params      :: [#cmd_param_spec{}]
}).



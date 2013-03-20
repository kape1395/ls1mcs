
%%
%%  Address, 3 bits.
%%
-type ls1p_addr() :: arm | arduino | eps | gps.


%%
%%  Port, 4 bits.
%%  TODO: Use real endpoint port numbers.
%%
-type ls1p_port_arm()   :: ping | cmdlog.
-type ls1p_port_ard()   :: default.
-type ls1p_port_eps()   :: default.
-type ls1p_port_gps()   :: nmea | binary.
-type ls1p_port() ::
    ls1p_port_arm() |
    ls1p_port_ard() |
    ls1p_port_eps() |
    ls1p_port_gps().

%%
%%  Acknowledgement, 1 bit.
%%  In a command, indicates, wether an acknowledgement should be sent for the command.
%%  In a data frame, 1 indicates an acknowledgement.
%%
-type ls1p_ack() :: boolean().


%%
%%  Correlation reference, 16 bits.
%%
-type ls1p_cref() :: integer().


%%
%%  Command execution delay in seconds, 16 bits.
%%
-type ls1p_delay() :: integer().


%%
%%  Fragment number, 16 bits.
%%
-type ls1p_fragment() :: integer().


%%
%%  Payload, 16 bits.
%%
-type ls1p_payload() :: binary().


%%
%%  Frames.
%%  TODO: Delay. Maybe add arm_scheduler as a destination address?
%%
-record(ls1p_cmd_frame, {
    dest_addr   :: ls1p_addr(),
    dest_port   :: ls1p_port(),
    ack = true  :: ls1p_ack(),
    cref        :: ls1p_cref(),
    delay = 0   :: ls1p_delay(),
    data = <<>> :: ls1p_payload()
}).

-record(ls1p_dat_frame, {
    src_addr    :: ls1p_addr(),
    src_port    :: ls1p_port(),
    ack         :: ls1p_ack(),
    cref        :: ls1p_cref(),
    fragment    :: ls1p_fragment(),
    data        :: ls1p_payload()
}).


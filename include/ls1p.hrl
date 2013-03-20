
%%
%%  Address, 3 bits.
%%
-type ls1p_addr() :: arm | arduino | eps | gps_nmea | gps_bin.


%%
%%  Port, 5 bits.
%%  TODO: Use real endpoint port numbers.
%%
-type ls1p_port_arm()      :: ping | cmdlog.
-type ls1p_port_arduino()  :: default.
-type ls1p_port_eps()      :: default.
-type ls1p_port_gps_nmea() :: default.
-type ls1p_port_gps_bin()  :: default.
-type ls1p_port() ::
    default |
    ls1p_port_arm() |
    ls1p_port_arduino() |
    ls1p_port_eps() |
    ls1p_port_gps_nmea() |
    ls1p_port_gps_bin().


%%
%%  Correlation reference, 16 bit.
%%
-type ls1p_cref() :: integer().


%%
%%  Fragment number, 16 bit.
%%
-type ls1p_fragment() :: integer().


%%
%%  Payload, 16 bit.
%%
-type ls1p_payload() :: binary().


%%
%%  Frames.
%%
-record(ls1p_cmd_frame, {
    dest_addr,  :: ls1p_addr(),
    dest_port,  :: ls1p_port(),
    cref        :: ls1p_cref(),
    data        :: ls1p_payload()
}).

-record(ls1p_dat_frame, {
    src_addr    :: ls1p_addr(),
    src_port    :: ls1p_port(),
    cref        :: ls1p_cref(),
    fragment    :: ls1p_fragment(),
    data        :: ls1p_payload()
}).


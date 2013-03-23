
%%
%%  Address, 3 bits.
%%
-type ls1p_addr() :: arm | arduino | eps | gps | helium.


%%
%%  Port, 4 bits.
%%  TODO: Use real endpoint port numbers?
%%
-type ls1p_port_arm()       :: ping | cmd_log | cmd_kill | tm_archive | tm_realtime | gps_log_bin | gps_log_nmea.
-type ls1p_port_arduino()   :: default.
-type ls1p_port_eps()       :: default.
-type ls1p_port_gps()       :: nmea | binary.
-type ls1p_port_helium()    :: default.
-type ls1p_port() ::
    ls1p_port_arm() |
    ls1p_port_arduino() |
    ls1p_port_eps() |
    ls1p_port_gps() |
    ls1p_port_helium().

%%
%%  Acknowledgement request, 1 bit.
%%  Indicates, wether an acknowledgement should be sent for the command.
%%
-type ls1p_ack() :: boolean().


%%
%%  End of File, 1 bit.
%%  Indicates, if the data frame is the last in the stream.
%%
-type ls1p_eof() :: boolean().


%%
%%  Command status, 1 bit.
%%  1 indicates success.
%%
-type ls1p_status() :: boolean().


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
%%  Command reception status, 8 bits.
%%  0 indicates success.
%%
-type ls1p_recv_status() :: integer().


%%
%%  Payload, 16 bits.
%%
-type ls1p_payload() :: binary().


%%
%%  Command frame.
%%
-record(ls1p_cmd_frame, {
    dest_addr   :: ls1p_addr(),
    dest_port   :: ls1p_port(),
    ack = true  :: ls1p_ack(),
    cref        :: ls1p_cref(),
    delay = 0   :: ls1p_delay(),
    data = <<>> :: ls1p_payload()
}).

%%
%%  Data frame, sent from SAT to GS.
%%
-record(ls1p_dat_frame, {
    src_addr    :: ls1p_addr(),
    src_port    :: ls1p_port(),
    eof         :: ls1p_eof(),
    cref        :: ls1p_cref(),
    fragment    :: ls1p_fragment(),
    data        :: ls1p_payload()
}).

%%
%%  Acknowledgement frame, sent from SAT to GS.
%%  This frame type is recognized by its length (4 octets).
%%
-record(ls1p_ack_frame, {
    src_addr    :: ls1p_addr(),
    src_port    :: ls1p_port(),
    status      :: ls1p_status(),
    cref        :: ls1p_cref(),
    recv_status :: ls1p_recv_status()
}).


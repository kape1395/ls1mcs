-ifndef(LS1P_HRL).
-define(LS1P_HRL, 1).


%%
%%  Address, 3 bits.
%%
-type ls1p_addr() :: arm | arduino | eps | gps | helium | ground.


%%
%%  Port, 4 bits.
%%
-type ls1p_port_arm()       :: ping | kill | downlink | runtime_tm | job_period | multi.
-type ls1p_port_arduino()   :: take_photo | photo_meta | photo_data.
-type ls1p_port_eps()       :: command.
-type ls1p_port_gps()       :: nmea | binary.
-type ls1p_port_helium()    :: command.
-type ls1p_port_ground()    :: ack | data | telemetry.
-type ls1p_port() ::
    ls1p_port_arm() |
    ls1p_port_arduino() |
    ls1p_port_eps() |
    ls1p_port_gps() |
    ls1p_port_helium() |
    ls1p_port_ground().

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
    addr        :: ls1p_addr(),     %% Destination address
    port        :: ls1p_port(),     %% Destination port
    ack = true  :: ls1p_ack(),
    cref        :: ls1p_cref(),
    delay = 0   :: ls1p_delay(),
    data = <<>> :: ls1p_payload()
}).

%%
%%  Acknowledgement frame, sent from SAT to GS.
%%  This frame type is recognized by its length (4 octets).
%%
-record(ls1p_ack_frame, {
    status      :: ls1p_status(),
    cref        :: ls1p_cref(),
    recv_status :: ls1p_recv_status()
}).

%%
%%  Data frame, sent from SAT to GS.
%%
-record(ls1p_data_frame, {
    eof         :: ls1p_eof(),
    cref        :: ls1p_cref(),
    fragment    :: ls1p_fragment(),
    data = <<>> :: ls1p_payload()
}).

%%
%%  Telemetry frame, sent from SAT to GS.
%%
-record(ls1p_tm_frame, {
    timestamp   :: os:timestamp(),      %% Timestamp (erlang:now()), when the frame was received.
    data = <<>> :: ls1p_payload()       %% TM data, including SAT's timestamp.
}).



-endif.

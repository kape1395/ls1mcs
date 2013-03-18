-record(ls1p_cmd_header, {addr, cmd_code, ref}).
-record(ls1p_data_header, {src_addr, msg_code, ref, no}).



-record(ax25_addr, {
    call    :: list(),
    ssid    :: integer()
}).
-record(ax25_frame, {
    dst     :: #ax25_addr{},
    src     :: #ax25_addr{},
    data    :: binary()
}).


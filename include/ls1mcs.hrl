
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


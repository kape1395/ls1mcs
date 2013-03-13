

Example:

    l(gen_ax25u).
    gen_ax25u:start({local, none}, "1", "asd", []).
    gen_ax25u:info(none).
    gen_ax25u:stop(none).

Using valgrind:

    gen_ax25u:start({local, none}, "1", "asd", [{port_proxy, "/usr/bin/valgrind"}]).


Erlang
======

Example:

    l(gen_ax25u).
    gen_ax25u:start({local, none}, "1", "asd", []).
    gen_ax25u:info(none).
    gen_ax25u:stop(none).

Using valgrind:

    gen_ax25u:start({local, none}, "1", "asd", [{port_proxy, "/usr/bin/valgrind"}]).

AX25
====

You can monitor ax25 traffix by the following command:

    sudo axlisten -a -p 1

One can use the following calls for testing:

  * LY2EN - Simonas
  * LY1BVB - VU HAM Club


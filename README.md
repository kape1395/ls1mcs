
Erlang
======

Example:

    erl -pa ebin
        l(gen_ax25u).
        gen_ax25u:start({local, none}, "3", "LY1BWB", []).
        gen_ax25u:info(none).
        gen_ax25u:stop(none).
        gen_ax25u:send(none, <<"labas">>).

Using valgrind:

    gen_ax25u:start({local, none}, "3", "LY1BWB", [{port_proxy, "/usr/bin/valgrind"}]).

AX25
====

You can monitor ax25 traffix by the following command:

    sudo axlisten -a -p 1

One can use the following calls for testing:

  * LY2EN - Simonas
  * LY1BVB - VU HAM Club

libax25
=======

See `using socket udp pthread`
[broadcast](http://stackoverflow.com/questions/6374122/network-udp-broadcast-design)
[sock and pthreads](http://www.cs.allegheny.edu/sites/cs440S2007/12)

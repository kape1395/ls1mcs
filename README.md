
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

> Sveiki,
> 
> galimas daiktas kad kažkokia ax25 biblioteka 'sulaužė' kernelį:
> 
> `Mar 15 06:29:17 raimis-desktop kernel: [851245.416675] gen_ax25u_port[28277]: segfault at 127 ip 00539a62 sp bfc08384 error 4 in libpthread-2.11.1.so[533000+15000]`
> 
> perkrovė, dabar veikia.
> 
> 
> ----
> Raimundas
> 
> On 2013 03 16, at 06:39 , Karolis Petrauskas wrote:
> 
> Sveiki,
>
>    Panašu, kad serveris (85.195.154.49) nebepasiekiamas...
>
> Karolis


Plain RS-232
============

The following erlang applications were found while looking for a library allowing
to access COM ports:

  * [erlang-serial](https://github.com/tonyg/erlang-serial),
  * [cereal](https://github.com/joewilliams/cereal),
  * [srly](https://github.com/msantos/srly),
  * [uart](https://github.com/tonyrog/uart).

For the first attempt, the `uart` application was choosen.

`uart`
------

Some bug in the `uart` application. Driver is not linked with `libutil` by default.
As a workaround, one should compile this application as follows:

    env LDFLAGS=-lutil rebar compile

    env ERL_LIBS=deps erl
        application:start(uart).
        {ok, U} = uart:open("/dev/ttyS0", []).
        uart:send(U, "asd").
        uart:recv(U, 1, 1000).

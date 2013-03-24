
Erlang
======

Example:

    erl -pa ebin
        l(gen_ax25u).
        gen_ax25u:start({local, none}, "3", "LY1BWB", []).
        gen_ax25u:info(none).
        gen_ax25u:stop(none).
        gen_ax25u:send(none, <<"labas">>).
        gen_ax25u:send(none, <<0, 0, 0, 123, 2, 3, 1>>).

    ./priv/gen_ax25u_port recv 3 LY2EN
    ./priv/gen_ax25u_port recv 4 LY1BWB

Using valgrind:

    gen_ax25u:start({local, none}, "3", "LY1BWB", [{port_proxy, "/usr/bin/valgrind"}]).

Starting application for tests:

    env ERL_LIBS=deps erl -config test/test -pa ebin/
        ls1mcs:start().
        ls1mcs_protocol:send({ls1mcs_proto_kiss, {n, l, ls1mcs_proto_kiss}}, <<"labas">>).
        ls1mcs_protocol:send({ls1mcs_proto_ax25, {n, l, ls1mcs_proto_ax25}}, <<"labas">>).



AX25
====

You can monitor ax25 traffix by the following command:

    sudo axlisten -a -p 1

One can use the following calls for testing:

  * LY2EN - Simonas
  * LY1BWB - VU HAM Club

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

    env LDFLAGS=-lutil rebar compile

    env ERL_LIBS=deps erl
        application:start(uart).
        {ok, U} = uart:open("/dev/ttyS0", []).
        % {ok, U} = uart:open("/dev/tnt2", []). % Session one.
        % {ok, U} = uart:open("/dev/tnt3", []). % Session two.
        uart:send(U, "asd").
        uart:recv(U, 1, 1000).


    sudo chmod o+rw /dev/tnt2 /dev/tnt3



Known problems:

  * Some bug in the `uart` application. Driver is not linked with `libutil` by default.
    As a workaround, one should compile this application as follows:
  * A lot of warnings due to missing `#include <string.h>`



Soundmodem
==========

Soundmodem is usefull for testing this software without real TNC.
See [Soundmodem HOWTO](http://www.xastir.org/wiki/HowTo:SoundModem) for more details.
The following are the instructions I used to setup the soundmodem in the KISS mode
(com port) on my PC. Just install the `soundmodem` and configure it properly:

    sudo apt-get install soundmodem
    sudo /usr/bin/soundmodemconfig # See config values bellow. Main: alsa, plughw:0,0, KISS

The `/etc/ax25/soundmodem.conf` contents are as follows (formatted here):

    <?xml version="1.0"?>
    <modem>
        <configuration name="KISS">
            <chaccess txdelay="150" slottime="100" ppersist="40" fulldup="0" txtail="10"/>
            <audio type="alsa" device="plughw:0,0" halfdup="1" capturechannelmode="Mono"/>
            <ptt file="/dev/ttyS0" gpio="0" hamlib_model="" hamlib_params=""/>
            <channel name="Channel 0">
                <mod mode="afsk" bps="1200" f0="1200" f1="2200" diffenc="1"/>
                <demod mode="afsk" bps="1200" f0="1200" f1="2200" diffdec="1"/>
                <pkt mode="KISS" ifname="sm0" hwaddr="" ip="10.0.0.1" netmask="255.255.255.0" broadcast="10.0.0.255" file="/dev/soundmodem0" unlink="1"/>
            </channel>
        </configuration>
    </modem>

To start the soundmodem, run the following as a root:

    sudo soundmodem -v 999 2>&1 | grep -v rx

To test the soundmodem, run the following:

    sudo chmod og+rw /dev/soundmodem0
    cat /root/aaa >> /dev/soundmodem0

Where the `aaa` file has one KISS frame:

    hexdump -C aaa 
    00000000  c0 00 62 61 73 c0                                 |..bas.|
    00000006



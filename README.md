
1. Erlang
=========

1.1. Building
-------------

Build everyging:

    sudo apt-get install gcc make g++ libpam0g-dev
    make deps
    make

then:

    mkdir -p temp/data/yaws/www
    env ERL_LIBS=deps erl -config priv/test-noc -pa ebin/ -eval 'ls1mcs:start().'   # No radio connection
    env ERL_LIBS=deps erl -config priv/test-snd -pa ebin/ -eval 'ls1mcs:start().'   # Connection via soundmodem
    env ERL_LIBS=deps erl -config priv/test-tnc -pa ebin/ -eval 'ls1mcs:start().'   # Connection via TNC2H-DK9SJ WA8DED Hostmode

Now you should be able to access [web ui](http://localhost:12321/).

To send some command via the communication link, run the following in the erlang shell:

    rr(ls1mcs_proto_ls1p).
    HMName = {n, l, ls1mcs_tnc_wa8ded_hm}, HMRef = ls1mcs_protocol:make_ref(ls1mcs_tnc_wa8ded_hm, HMName).
    LSName = {n, l, ls1mcs_proto_ls1p},    LSRef = ls1mcs_protocol:make_ref(ls1mcs_proto_ls1p, LSName).
    ls1mcs_protocol:send(HMRef, <<"labas">>).
    ls1mcs_protocol:send(LSRef, #ls1p_cmd_frame{dest_addr = arm, dest_port = cmd_log, ack = false, cref = 1259, delay = 0, data = <<2:16, 27:16>>}).
    ls1mcs_tnc_wa8ded_hm:invoke(HMName, <<"@D 1">>).  % Full duplex ON
    ls1mcs_tnc_wa8ded_hm:invoke(HMName, <<"@B">>).    % Show free memory
    ls1mcs_tnc_wa8ded_hm:invoke(HMName, <<"K">>).     % Show date
    ls1mcs_tnc_wa8ded_hm:invoke(HMName, <<"L">>).     % Show channel
    ls1mcs_tnc_wa8ded_hm:invoke(HMName, <<"H">>).     % Show heard list


1.2. Running
------------

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

    # mkdir -p temp/data/yaws/www
    # env ERL_LIBS=deps erl -config test/test-nocon -pa ebin/
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



TNC2H-DK9SJ
===========

The following commands can be used to interface with the TNC manually:

  * `minicom`
  * `cu`


    cu -l /dev/ttyUSB0 -s 9600
    cu -l /dev/ttyUSB0 -s 9600 --nostop --parity=none
        <ESC> L
        <ESC> S 0
        <ESC> I LY2EN


    {ok, Port} = uart:open("/dev/ttyUSB0", [{baud, 9600}, {csize, 7}]).
    uart:send(Port, <<16#1b>>).
    uart:send(Port, <<27, $L, 10, 13>>).

    uart:recv(Port, 1, 1000).


    {ok, Port} = uart:open("/dev/ttyUSB0", [{baud, 9600}, {csize, 8}, {parity, none}, {mode, binary}]).
    uart:send(Port, <<27, "L", 13>>).
    %art:send(Port, <<27, "S 0", 13>>).     %% Sita ivykdzius jau pradeda snypsti...
    uart:send(Port, <<27, "I LY2EN", 13>>).
    uart:send(Port, <<27, "JHOST 1", 13>>).
    uart:send(Port, <<192, 0, "Test message", 192>>).
    uart:send(Port, <<192, 0, "Test message", 192, 10, 13>>).
    %%
    %%  Now the TNC seems to be in the KISS mode (giving no response to the L command).
    %%  Being in 9600, audio loopback mode it sends all the data back to the terminal.
    %%  When echoing data, the most significant bit in each byte seems zeroed.
    %%
    uart:recv(Port, 1, 100).
    uart:recv(Port, 10, 100).
    uart:close(Port).


[KISS](http://he.fi/archive/linux-hams/199911/0146.html)
> stty 9600 < /dev/ttyS0 > /dev/ttyS0 (9600 is the baudrate between
> PC and TNC)
> echo -e "\033@KISS ON\r" < /dev/ttyS0 > /dev/ttyS0
> kissattach etc...
[Script](http://he.fi/archive/linux-hams/199911/0147.html)
[Other](http://marc.info/?l=suse-ham&m=110885103018627)


According to the [WA8DED manual](http://www.ir3ip.net/iw3fqg/doc/wa8ded.htm),
the host mode is not the same as KISS mode. The host mode protocol is described
in the manual. Here is a working example, how to use it:

    {ok, Port} = uart:open("/dev/ttyUSB0", [{baud, 9600}, {csize, 8}, {parity, none}, {mode, binary}]).
    uart:send(Port, <<17, 24, 27, "JHOST 1", 13>>).
    uart:send(Port, <<0, 0, 4, "Labas">>).
    uart:send(Port, <<0, 1, 6, "JHOST 0">>).
    uart:send(Port, <<27, "QRES", 13>>).




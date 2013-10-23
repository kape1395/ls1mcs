Intruduction
============================================================

This is the MCS for the first lithuanian satellite "Lituanica SAT-1".



Erlang
============================================================

Building
------------------------------------------------------------

Build everyging:

    sudo apt-get install gcc make g++ libpam0g-dev
    make deps
    make

Generate a release and install it:

    make release
    cp -r rel/ls1mcs ~/



Running
------------------------------------------------------------

For testing purposes, without building a release:

    rm -rf temp
    mkdir -p temp/data/mnesia/db temp/data/yaws/www temp/data/gpredict
    env ERL_LIBS=deps erl -pa ebin/ -config priv/test-void_link -eval 'ls1mcs_utl_test:start().'     # No radio connection, all commands to log
    env ERL_LIBS=deps erl -pa ebin/ -config priv/test-file_link -eval 'ls1mcs_utl_test:start().'     # No radio connection, all commands to file
    env ERL_LIBS=deps erl -pa ebin/ -config priv/test-snd_modem -eval 'ls1mcs_utl_test:start().'     # Connection via soundmodem
    env ERL_LIBS=deps erl -pa ebin/ -config priv/test-wa8ded_hm -eval 'ls1mcs_utl_test:start().'     # Connection via TNC2H-DK9SJ WA8DED Hostmode
    env ERL_LIBS=deps erl -pa ebin/ -config priv/test-tapr_kiss -eval 'ls1mcs_utl_test:start().'     # Connection via TNC2H-DK9SJ TAPR KISS mode
    env ERL_LIBS=deps erl -pa ebin/ -config priv/test-mfj1270c_kiss -eval 'ls1mcs_utl_test:start().' # Connection via MFJ-1270C using KISS mode


Now you should be able to access [web ui](http://localhost:8080/).
To send some command via the communication link, run the following in the erlang shell:

    rr(ls1mcs_proto_ls1p).
    HMName = {n, l, ls1mcs_tnc_wa8ded_hm}, HMRef = ls1mcs_protocol:make_ref(ls1mcs_tnc_wa8ded_hm, HMName).
    TKName = {n, l, ls1mcs_tnc_tapr_kiss}, TKRef = ls1mcs_protocol:make_ref(ls1mcs_tnc_tapr_kiss, TKName).
    KSName = {n, l, ls1mcs_proto_kiss},    KSRef = ls1mcs_protocol:make_ref(ls1mcs_proto_kiss, KSName).
    AXName = {n, l, ls1mcs_proto_ax25},    AXRef = ls1mcs_protocol:make_ref(ls1mcs_proto_ax25, AXName).
    LSName = {n, l, ls1mcs_proto_ls1p},    LSRef = ls1mcs_protocol:make_ref(ls1mcs_proto_ls1p, LSName).
    ls1mcs_protocol:send(HMRef, <<"labas">>).                 % HostMode: over AX25
    ls1mcs_protocol:send(KSRef, <<"labas">>).                 % KissMode: over KISS
    ls1mcs_protocol:send(AXRef, <<"labas">>).                 % KissMode: over AX25
    ls1mcs_protocol:send(LSRef, #ls1p_cmd_frame{dest_addr = arm, dest_port = cmd_log, ack = false, cref = 1259, delay = 0, data = <<2:16, 27:16>>}).
    ls1mcs_tnc_wa8ded_hm:invoke(HMName, <<"@D">>).    % Show half (0) / full (1) duplex mode
    ls1mcs_tnc_wa8ded_hm:invoke(HMName, <<"@D 1">>).  % Full duplex ON
    ls1mcs_tnc_wa8ded_hm:invoke(HMName, <<"@B">>).    % Show free memory
    ls1mcs_tnc_wa8ded_hm:invoke(HMName, <<"K">>).     % Show date
    ls1mcs_tnc_wa8ded_hm:invoke(HMName, <<"L">>).     % Show channel
    ls1mcs_tnc_wa8ded_hm:invoke(HMName, <<"H">>).     % Show heard list
    ls1mcs_tnc_wa8ded_hm:invoke(HMName, <<"M">>).     % Show monitoring filter?
    ls1mcs_tnc_wa8ded_hm:invoke(HMName, <<"M U">>).   % Set monitoring mode for UI.
    ls1mcs_tnc_wa8ded_hm:invoke(HMName, <<"E">>).     % Show echo status
    ls1mcs_tnc_wa8ded_hm:invoke(HMName, <<"E 0">>).   % Echo OFF
    ls1mcs_tnc_wa8ded_hm:invoke(HMName, <<"QRES">>).  % RESET
    ls1mcs_tnc_tapr_kiss:reenter_kiss_mode(TKName).

    lager:set_loglevel(lager_console_backend, warning).

    ls1mcs_utl_test:send_ping().
    ls1mcs_store:add_ls1p_frame(#ls1p_tm_frame{data = <<0:(233*8)>>}, <<2#11100100:8, 0:(233*8)>>, erlang:now()).

    {ok, FrameId} = ls1mcs_utl_test:send_photo_data(200, 0, 50000).
    ls1mcs_utl_test:load_photo(FrameId, 200, 0, 50000, "test/data/photo.jpg").
    % See http://localhost:8000/ls1mcs/api/ls1p_frame/0_7/photo

    ls1mcs_utl_test:load_photo_frames("temp-photo/20130727210211.log", 200, LSRef).

Test using user commands:

    ls1mcs_usr_cmd:issue(#usr_cmd{spec = ping}).
    ls1mcs_usr_cmd:issue(#usr_cmd{spec = job_period, args = [#usr_cmd_arg{name = jobid, value = <<"0">>}, #usr_cmd_arg{name = interval, value = <<"5">>}]}).
    ls1mcs_usr_cmd:issue(#usr_cmd{spec = take_photo, args = [#usr_cmd_arg{name = resid, value = <<"0">>}, #usr_cmd_arg{name = delay, value = <<"0">>}]}).
    ls1mcs_usr_cmd:issue(#usr_cmd{spec = photo_data, args = [#usr_cmd_arg{name = blksz, value = <<"100">>}, #usr_cmd_arg{name = from, value = <<"0">>}, #usr_cmd_arg{name = till, value = <<"10">>}]}).



Sending tele-commands
------------------------------------------------------------

Bytes received from He100 using `rp(ls1mcs_utl_uart_logger:recv(true)).`:

    ls1mcs_protocol:send(LSRef, #ls1p_cmd_frame{dest_addr = arm, dest_port = ping, ack = false, cref = 1259, delay = 0, data = <<>>}).
    %% {ok, <<72,101,32,4,0,23,59,163,134,162,64,64,64,64,224,152,178,100,138,156,64,97,3,240,0,4,235,0,0,53,208,125,141,72,101,32,4,255,255,34,137>>}
    %% See `test/data/test_he100_recv_ls1p_arm_ping.dat`.

    ls1mcs_protocol:send(LSRef, #ls1p_cmd_frame{dest_addr = arduino, dest_port = take_photo, ack = true, cref = 1260, delay = 30, data = <<0:16>>}).
    %% {ok, <<72,101,32,4,0,25,61,165,134,162,64,64,64,64,224,152,178,100,138,156,64,97,3,240,33,4,236,0,30,0,0,212,72,218,155,72,101,32,4,255,255,34,137>>}
    %% See `test/data/test_he100_recv_ls1p_arduino_take_photo.dat`.

    ls1mcs_protocol:send(LSRef, #ls1p_cmd_frame{dest_addr = arduino, dest_port = photo_meta, ack = false, cref = 1261, delay = 0, data = <<>>}).
    %% {ok, <<72,101,32,4,0,23,59,163,134,162,64,64,64,64,224,152,178,100,138,156,64,97,3,240,34,4,237,0,0,245,112,1,165>>}
    %% See `test/data/test_he100_recv_ls1p_arduino_photo_meta.dat`.

    %% TODO: The following is outdated.
    ls1mcs_protocol:send(LSRef, #ls1p_cmd_frame{dest_addr = arduino, dest_port = photo_data, ack = false, cref = 1262, delay = 0, data = <<0:16, 78:16>>}).
    %% {ok, <<72,101,32,4,0,27,63,167,134,162,64,64,64,64,224,152,178,100,138,156,64,97,3,240,36,4,238,0,0,0,0,0,78,113,194,44,196>>}
    %% See `test/data/test_he100_recv_ls1p_arduino_photo_data.dat`

REST API
========


    POST http://localhost:8000/ls1mcs/api/command/immediate
    {
      "spec": "ping"
    }
    {
      "spec": "take_photo",
      "args": [
        {"name": "resid", "value": 1},
        {"name": "delay", "value": 5}
      ]
    }
    {
      "spec": "photo_meta"
    }
    {
      "spec": "photo_data",
      "args": [
        {"name": "blksz", "value": 195},
        {"name": "from", "value": 0},
        {"name": "till", "value": 512}
      ]
    }
    {
      "spec": "dlnk_photo"
    }
    {
      "spec": "downlink",
      "args": [
        {"name": "bufid", "value": 0},
        {"name": "blksz", "value": 120},
        {"name": "from", "value": 13},
        {"name": "till", "value": 73}
      ]
    }



Communication options
=====================


TNC2h-WA8DED-HostMode
---------------------

### Experiment 1: 2013-07-19T18:50:00

  * TNC Pins: 101010000
  * Config: priv/test-wa8ded_hm
  * Incoming: works.
  * Outgoing: works.
  * Notes:
      * He100->GS: Command to He100 was sent using `python hee.py "Sveikinimai nuo kosmonautu\!"`
      * GS->He100: sent using `ls1mcs_protocol:send(HMRef, <<"Linkejimai is zemes stoties.">>).`,
        response logged with `ls1mcs_utl_uart_logger:start_link("/dev/ttyUSB1").` and
        written to `test/data/test_he100_rev_text.dat`.
      * TNC: "M N" was by default.
      * TNC: "M U" needs to be invoked.
      * TNC: Echo is received when sending messages with "M U" enabled event with "E 0".
      * Team supported by a inspirational bag of Sourcream and Onion chips!



TNC2h-TAPR-KISS
---------------


Soundmodem-KISS
---------------




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

    # sudo soundmodem -v 999 2>&1 | grep -v rx
    sudo priv/ls1mcs_snd_modem start
    sudo priv/ls1mcs_snd_modem stop
    sudo priv/ls1mcs_snd_modem status

To test the soundmodem, run the following:

    sudo chmod og+rw /dev/soundmodem0  # Not needed if started using priv/ls1mcs_snd_modem
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




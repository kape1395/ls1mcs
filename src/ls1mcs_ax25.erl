%%
%%  Implementation of a subset of the AX.25 protocol.
%%  See http://www.ax25.net/AX25.2.2-Jul%2098-2.pdf
%%  Also http://www.aero.iitb.ac.in/pratham/otherdocs/Comm%20Saurabh.pdf
%%
%%  Only UI frames are supported.
%%
-module(ls1mcs_ax25).
-export([bitstuff/1, bitdestuff/1, calculate_fcs/1]).

%%
%%  Does bitstuffing, as described in section 3.6 of
%%  the http://www.ax25.net/AX25.2.2-Jul%2098-2.pdf.
%%
bitstuff(<<2#11111:5, Rest/bitstring>>) ->
    RestStuffed = bitstuff(Rest),
    <<2#111110:6, RestStuffed/bitstring>>;

bitstuff(<<Head:1, Rest/bitstring>>) ->
    RestStuffed = bitstuff(Rest),
    <<Head:1, RestStuffed/bitstring>>;

bitstuff(<<>>) ->
    <<>>.


%%
%%  Un-does bitstuffing, as described in section 3.6 of
%%  the http://www.ax25.net/AX25.2.2-Jul%2098-2.pdf.
%%
bitdestuff(<<2#111110:6, Rest/bitstring>>) ->
    RestDestuffed = bitdestuff(Rest),
    <<2#11111:5, RestDestuffed/bitstring>>;

bitdestuff(<<Head:1, Rest/bitstring>>) ->
    RestDestuffed = bitdestuff(Rest),
    <<Head:1, RestDestuffed/bitstring>>;

bitdestuff(<<>>) ->
    <<>>.


%%
%%  Uses CRC-CCITT algorithm. The implementation is based on algorithm
%%  provided here: http://srecord.sourceforge.net/crc16-ccitt.html.
%%
%%  Other links:
%%  http://n1vg.net/packet/index.php
%%  http://www.billnewhall.com/TechDepot/AX25CRC/CRC_for_AX25.pdf
%%  http://globalengineer.wordpress.com/2008/09/16/my-first-erlang-module-part-1/
%%  http://globalengineer.wordpress.com/2008/09/24/my-first-erlang-module-part-2/
%%
-define(CRC16_INIT, 16#FFFF).
-define(CRC16_POLY, 16#1021).
-define(CRC16_AUGM, 16#0000).

calculate_fcs(Data) when is_list(Data) ->
    calculate_fcs(list_to_binary(Data));

calculate_fcs(Data) when is_binary(Data) ->
    Augmented = <<Data/binary, ?CRC16_AUGM:16>>,
    calculate_fcs(?CRC16_INIT, Augmented).


calculate_fcs(CRC, <<>>) ->
    CRC;

calculate_fcs(CRC, <<First:1, Rest/bitstring>>) ->
    ShiftedCRC = ((CRC bsl 1) band 16#FFFF) + First,
    UpdatedCRC = case ((CRC band 16#8000) =/= 0) of
        true -> ShiftedCRC bxor ?CRC16_POLY;
        false -> ShiftedCRC
    end,
    calculate_fcs(UpdatedCRC, Rest).


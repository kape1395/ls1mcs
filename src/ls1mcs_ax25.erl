%%
%%  Implementation of a subset of the AX.25 protocol.
%%  See http://www.ax25.net/AX25.2.2-Jul%2098-2.pdf [1]
%%  Also http://www.aero.iitb.ac.in/pratham/otherdocs/Comm%20Saurabh.pdf
%%
%%  Only UI frames are supported.
%%
-module(ls1mcs_ax25).
-export([bitstuff/1, bitdestuff/1, calculate_fcs/1, encode/3]).

-define(AX25_FLAG, 2#01111110). %% Denotes start and end of an AX.25 frame.
-define(AX25_PID_NOL3, 16#F0).  %% No Layer 3 Protocol

-record(addr, {call, ssid}).


%% =============================================================================
%%  Public API
%% =============================================================================

%% =============================================================================
%%  Internal data structures.
%% =============================================================================

%% =============================================================================
%%  Callbacks for gen_???.
%% =============================================================================


%% =============================================================================
%%  Internal Functions.
%% =============================================================================


%%
%%  DstAddr = #addr{call = "LY2EN", ssid=0},
%%  SrcAddr = #addr{call = "LY1BVB", ssid=0},
%%
encode(DstAddr, SrcAddr, Data) ->
    EncodedDstAddr = encode_address(DstAddr, 1, 0), %% Command, Dest address.
    EncodedSrcAddr = encode_address(SrcAddr, 0, 1), %% Command, Source addr.
    Address = reverse_bits(<<EncodedDstAddr/binary, EncodedSrcAddr/binary>>),

    ControlM1 = 2#000,  %% UI frame, see [1], section 4.3.3.
    ControlM2 = 2#00,   %% UI frame, see [1], section 4.3.3.
    ControlPF = 2#0,    %% Not used, see [1], sections 4.3.3, 4.3.3.6, 6.2.
    Control = reverse_bits(<<ControlM1:3, ControlPF:1, ControlM2:2, 2#11:2>>),

    Info = reverse_bits(Data),
    FrameContents = <<
        Address/binary,
        Control/binary,
        ?AX25_PID_NOL3:8,
        Info/binary
    >>,

    %%  Checksum: see [1], section 4.4.6;
    %%  And http://www.billnewhall.com/TechDepot/AX25CRC/CRC_for_AX25.pdf
    %%  FCS should not be bit-reversed.
    FCS = calculate_fcs(FrameContents),
    FrameWithFCS = <<
        FrameContents/binary,
        FCS:16
    >>,

    %%  Do bitstuffing and add flags.
    BitstuffedFrame = bitstuff(FrameWithFCS),
    <<?AX25_FLAG:8, BitstuffedFrame/binary, ?AX25_FLAG:8>>.

%%
%%  SSID: http://aprs.org/aprs11/SSIDs.txt
%%
encode_address(#addr{call = Call, ssid = SSID}, CBit, LastExtBit) ->
    BinCall = list_to_binary(Call),
    <<A1:8, A2:8, A3:8, A4:8, A5:8, A6:8>> = encode_address_pad(BinCall, 6, 16#20),
    <<
        A1:7, 0:1, A2:7, 0:1, A3:7, 0:1,
        A4:7, 0:1, A5:7, 0:1, A6:7, 0:1,
        CBit:1, 2#11:2, SSID:4, LastExtBit:1
    >>.

encode_address_pad(Addr, Len, Pad) when size(Addr) < Len ->
    <<Addr/binary, Pad:8>>;

encode_address_pad(Addr, Len, _) when size(Addr) == Len ->
    Addr.


%%
%%  Reverse bits in each octet.
%%
reverse_bits(<<B0:1, B1:1, B2:1, B3:1, B4:1, B5:1, B6:1, B7:1, Rest/binary>>) ->
    ReversedRest = reverse_bits(Rest),
    <<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1, ReversedRest/binary>>.


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
%%  Uses  algorithm. The implementation is based on algorithm
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


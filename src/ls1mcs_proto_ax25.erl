%%
%%  Implementation of a subset of the AX.25 protocol.
%%  See [1:Specification](http://www.ax25.net/AX25.2.2-Jul%2098-2.pdf),
%%  [2:Example impl](http://www.webalice.it/capaso/DOCS/APRS/dcc.pdf),
%%  also (http://www.aero.iitb.ac.in/pratham/otherdocs/Comm%20Saurabh.pdf).
%%
%%  Only UI frames are supported.
%%
-module(ls1mcs_proto_ax25).
-behaviour(gen_server).
-behaviour(ls1mcs_protocol).
-export([start_link/5, start_link/6, send/2, received/2]).
-export([bitstuff/1, bitdestuff/1, calculate_fcs/1, encode/2, decode/2, split_frames/1, parse_call/1]). % For tests
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("ls1mcs.hrl").

-define(MAX_FRAME_LEN, 1024).   %% Maximal frame length (before forced drop).
-define(AX25_FLAG, 2#01111110). %% Denotes start and end of an AX.25 frame.
-define(AX25_PID_NOL3, 16#F0).  %% No Layer 3 Protocol
-define(ADDR_RR_UNUSED, 2#11).  %% Address, RR bits, when unused.
-define(CTRL_FRAME_U, 2#11).    %% Last 2 bits in the control field, indicating U frame.



%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%  Name - .
%%  Lower - .
%%  Upper - .
%%  Mode:
%%      std -- Standard AX25, as described in the spec.
%%      tnc -- AX.25 frame, as returned by the TNC2H in the TAPR KISS mode.
%%        - Have no frame flags
%%        - Have no bitstuffing.
%%        - Bits are not reversed.
%%
start_link(Name, Lower, Upper, Local, Remote, Mode) ->
    gen_server:start_link({via, gproc, Name}, ?MODULE, {Lower, Upper, Local, Remote, Mode}, []).


%%
%%  See start_link/4.
%%
start_link(Name, Lower, Upper, Local, Remote) ->
    gen_server:start_link({via, gproc, Name}, ?MODULE, {Lower, Upper, Local, Remote, std}, []).


%%
%%
%%
send(Ref, Data) when is_binary(Data) ->
    gen_server:cast({via, gproc, Ref}, {send, Data}).


%%
%%
%%
received(Ref, Data) when is_binary(Data) ->
    gen_server:cast({via, gproc, Ref}, {received, Data}).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    lower,      %% Lower protocol ref.
    upper,      %% Upper protocol ref.
    data,       %% Buffer for an input from the lower level.
    local,      %% Local call
    remote,     %% Remote call
    mode        %% Operation mode: std | tnc
}).


%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================


%%
%%
%%
init({Lower, Upper, Local, Remote, Mode}) ->
    self() ! {initialize},
    {ok, #state{
        lower = Lower,
        upper = Upper,
        data = <<>>,
        local = parse_call(Local),
        remote = parse_call(Remote),
        mode = Mode
    }}.


%%
%%
%%
handle_call(_Message, _From, State) ->
    {stop, not_implemented, State}.


%%
%%  Encode frame and send it to the lower protocol.
%%
handle_cast({send, Data}, State = #state{local = Local, remote = Remote, lower = Lower, mode = Mode}) ->
    Frame = #ax25_frame{
        dst = Remote,
        src = Local,
        data = Data
    },
    {ok, FrameBinary} = encode(Frame, Mode),
    ok = ls1mcs_protocol:send(Lower, FrameBinary),
    {noreply, State};

%%
%%  Decode frame and send its payload to the upper protocol.
%%
handle_cast({received, Received}, State = #state{upper = Upper, data = Collected, mode = std = Mode}) ->
    {Reminder, Frames} = split_frames(<<Collected/binary, Received/binary>>),

    %%  Decode all frames and sent them to the upper level.
    ReceivedFrameFun = fun (FrameBinary) ->
        case catch decode(FrameBinary, Mode) of
            {ok, #ax25_frame{data = FrameInfo}} ->
                ok = ls1mcs_protocol:received(Upper, FrameInfo);
            Error ->
                io:format("WARN: AX25: Ignoring bad frame: error = ~p, input=~p~n", [Error, FrameBinary])
        end
    end,
    lists:foreach(ReceivedFrameFun, Frames),

    %%  Ckeck if buffer is not accumulating to much of data.
    NewState = case size(Reminder) > ?MAX_FRAME_LEN of
        false ->
            State#state{data = Reminder};
        true ->
            <<StrippedReminder:?MAX_FRAME_LEN/binary, NewReminder/binary>> = Reminder,
            io:format("WARN: AX25: Buffer to big, several bytes dropped: input=~p~n", [StrippedReminder]),
            State#state{data = NewReminder}
    end,
    {noreply, NewState};

handle_cast({received, Received}, State = #state{upper = Upper, mode = tnc = Mode}) ->
    case catch decode(Received, Mode) of
        {ok, #ax25_frame{data = FrameInfo}} ->
            ok = ls1mcs_protocol:received(Upper, FrameInfo);
        Error ->
            io:format("WARN: AX25: Ignoring bad frame: error = ~p, input=~p~n", [Error, Received])
    end,
    {noreply, State}.

%%
%%
%%
handle_info({initialize}, State = #state{lower = Lower, upper = Upper}) ->
    ls1mcs_protocol:await(Upper),
    ls1mcs_protocol:await(Lower),
    {noreply, State}.


%%
%%
%%
terminate(_Reason, _State) ->
    ok.


%%
%%
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% =============================================================================
%%  Internal Functions.
%% =============================================================================


%%
%%  Parses Call Sign into #ax25_addr{}.
%%
parse_call(CallString) when is_list(CallString) ->
    case lists:member($-, CallString) of
        false ->
            #ax25_addr{call = CallString, ssid = 0};
        true ->
            SplitFun = fun (C) -> C =/= $- end,
            {Call, [$- | SSID]} = lists:splitwith(SplitFun, CallString),
            #ax25_addr{call = Call, ssid = list_to_integer(SSID)}
    end;

parse_call(Call) when is_record(Call, ax25_addr) ->
    Call.



%%
%%  Splits a binary into AX25 frames.
%%  The resulting frames include flags at both ends.
%%
split_frames(Data) ->
    FlagPos = binary:matches(Data, <<?AX25_FLAG:8>>),
    SplitFun = fun ({FlagStart, FlagLen}, {PrevStart, Frames}) ->
        FrameStart = PrevStart,
        FrameLen = FlagStart - PrevStart + FlagLen,
        FrameBin = binary:part(Data, FrameStart, FrameLen),
        NewFrames = case FrameBin of
            <<>> -> Frames;
            <<?AX25_FLAG:8>> -> Frames;
            <<?AX25_FLAG:8, ?AX25_FLAG:8>> -> Frames;
            _ -> [FrameBin | Frames]
        end,
        {FlagStart, NewFrames}
    end,
    {LastStart, Frames} = lists:foldl(SplitFun, {0, []}, FlagPos),
    Reminder = binary:part(Data, LastStart, size(Data) - LastStart),
    {Reminder, lists:reverse(Frames)}.


%%
%%  DstAddr = #addr{call = "LY2EN", ssid=0},
%%  SrcAddr = #addr{call = "LY1BWB", ssid=0},
%%
encode(#ax25_frame{dst = DstAddr, src = SrcAddr, data = Data}, std) ->
    EncodedDstAddr = encode_address(DstAddr, 1, 0), %% Command, Dest address.
    EncodedSrcAddr = encode_address(SrcAddr, 0, 1), %% Command, Source addr.
    Address = reverse_bits(<<EncodedDstAddr/binary, EncodedSrcAddr/binary>>),
    Control = reverse_bits(control_byte_ui()),
    Info = reverse_bits(Data),

    FrameContents = <<
        Address/binary,
        Control/binary,
        ?AX25_PID_NOL3:8,
        Info/binary
    >>,

    FCS = calculate_fcs(FrameContents),
    FrameWithFCS = <<FrameContents/binary, FCS:16>>,

    BitstuffedFrameContents = bitstuff(FrameWithFCS),
    {ok, <<?AX25_FLAG:8, BitstuffedFrameContents/binary, ?AX25_FLAG:8>>};

encode(#ax25_frame{dst = DstAddr, src = SrcAddr, data = Data}, tnc) ->
    EncodedDstAddr = encode_address(DstAddr, 1, 0), %% Command, Dest address.
    EncodedSrcAddr = encode_address(SrcAddr, 0, 1), %% Command, Source addr.
    Address = <<EncodedDstAddr/binary, EncodedSrcAddr/binary>>,
    Control = control_byte_ui(),

    FrameContents = <<
        Address/binary,
        Control/binary,
        ?AX25_PID_NOL3:8,
        Data/binary
    >>,

    FCS = calculate_fcs(FrameContents),
    FrameWithFCS = <<FrameContents/binary, FCS:16>>,
    {ok, FrameWithFCS}.


%%
%%
%%
decode(BitstuffedFrame, std) ->
    %%  Preliminary frame validation, see [1], section 3.9.
    FrameLen = bit_size(BitstuffedFrame),
    true = FrameLen >= 136,
    0 = FrameLen rem 8,

    %%  Validate and remove flags, undo bitstuffing and padding by 0.
    BFCLen = size(BitstuffedFrame) - 2,
    <<?AX25_FLAG:8, BitstuffedFrameContents:BFCLen/binary, ?AX25_FLAG:8>> = BitstuffedFrame,
    FrameWithFCS = bitdestuff(BitstuffedFrameContents),

    %%  Validate checksum.
    FCLen = size(FrameWithFCS) - 2,
    <<FrameContents:FCLen/binary, FCS:16>> = FrameWithFCS,
    FCS = calculate_fcs(FrameContents),

    %%  Parse two addresses.
    <<DstAddrBin:7/binary, SrcAddrBin:7/binary, ControlPidInfo/binary>> = FrameContents,
    {ok, DstCall, DstSSID, 0} = decode_address(reverse_bits(DstAddrBin)),
    {ok, SrcCall, SrcSSID, 1} = decode_address(reverse_bits(SrcAddrBin)), %% 1 Means no repeater addressed follow.

    %% Parse control byte (not two bytes), PID and Payload.
    <<_M1:3, _PF:1, _M2:2, ?CTRL_FRAME_U:2, _PID:8, Info/binary>> = reverse_bits(ControlPidInfo),

    {ok, #ax25_frame{
        dst = #ax25_addr{call = DstCall, ssid = DstSSID},
        src = #ax25_addr{call = SrcCall, ssid = SrcSSID},
        data = Info
    }}.


%%
%%  Address: see [1], section 3.12.2.
%%  SSID: see http://aprs.org/aprs11/SSIDs.txt
%%
encode_address(#ax25_addr{call = Call, ssid = SSID}, CBit, LastExtBit) ->
    BinCall = list_to_binary(Call),
    <<A1:8, A2:8, A3:8, A4:8, A5:8, A6:8>> = rpad(BinCall, 6, 16#20),
    <<
        A1:7, 0:1, A2:7, 0:1, A3:7, 0:1,
        A4:7, 0:1, A5:7, 0:1, A6:7, 0:1,
        CBit:1, ?ADDR_RR_UNUSED:2, SSID:4, LastExtBit:1
    >>.

decode_address(AddressBin) ->
    <<
        A1:7, 0:1, A2:7, 0:1, A3:7, 0:1,
        A4:7, 0:1, A5:7, 0:1, A6:7, 0:1,
        _C:1, _RR:2, SSID:4, LastExtBit:1
    >> = AddressBin,
    BinCall = rtrim(<<A1:8, A2:8, A3:8, A4:8, A5:8, A6:8>>, 16#20),
    {ok, binary_to_list(BinCall), SSID, LastExtBit}.


%%
%%  Appends Byte-s to Data to make it exactly Len bytes length.
%%
rpad(Data, Len, Byte) when size(Data) < Len ->
    rpad(<<Data/binary, Byte:8>>, Len, Byte);

rpad(Data, Len, _) when size(Data) == Len ->
    Data.


%%
%%
%%
rtrim(Data, Byte) ->
    Len = size(Data) - 1,
    case Data of
        <<>> -> <<>>;
        <<Byte:8>> -> <<>>;
        <<Prefix:Len/binary, Byte:8>> -> rtrim(Prefix, Byte);
        _ -> Data
    end.


%%
%%  Reverse bits in each octet.
%%
reverse_bits(<<>>) ->
    <<>>;

reverse_bits(<<B0:1, B1:1, B2:1, B3:1, B4:1, B5:1, B6:1, B7:1, Rest/binary>>) ->
    ReversedRest = reverse_bits(Rest),
    <<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1, ReversedRest/binary>>.


%%
%%  Does bitstuffing, as described in section 3.6 of
%%  the http://www.ax25.net/AX25.2.2-Jul%2098-2.pdf.
%%  Padding by 0 is added in order to leave data aligned with 8.
%%
bitstuff(Data) ->
    {BitsuffedData, StuffedCount} = bitstuff(Data, 0),
    PaddingLen = case StuffedCount rem 8 > 0 of
        true -> 8 - (StuffedCount rem 8);
        false -> 0
    end,
    <<BitsuffedData/bitstring, 0:PaddingLen>>.

bitstuff(<<2#11111:5, Rest/bitstring>>, Count) ->
    {RestStuffed, NewCount} = bitstuff(Rest, Count + 1),
    {<<2#111110:6, RestStuffed/bitstring>>, NewCount};

bitstuff(<<Head:1, Rest/bitstring>>, Count) ->
    {RestStuffed, NewCount} = bitstuff(Rest, Count),
    {<<Head:1, RestStuffed/bitstring>>, NewCount};

bitstuff(<<>>, Count) ->
    {<<>>, Count}.


%%
%%  Un-does bitstuffing, as described in section 3.6 of
%%  the http://www.ax25.net/AX25.2.2-Jul%2098-2.pdf.
%%  Padding by 0 is stripped here.
%%
bitdestuff(Data) ->
    {DestuffedData, DestuffedCount} = bitdestuff(Data, 0),
    PaddingLen = case DestuffedCount rem 8 > 0 of
        true -> 8 - (DestuffedCount rem 8);
        false -> 0
    end,
    ContentLen = bit_size(DestuffedData) - PaddingLen,
    <<DestuffedUnpaddedData:ContentLen/bitstring, 0:PaddingLen>> = DestuffedData,
    DestuffedUnpaddedData.


bitdestuff(<<2#111110:6, Rest/bitstring>>, Count) ->
    {RestDestuffed, NewCount} = bitdestuff(Rest, Count + 1),
    {<<2#11111:5, RestDestuffed/bitstring>>, NewCount};

bitdestuff(<<Head:1, Rest/bitstring>>, Count) ->
    {RestDestuffed, NewCount} = bitdestuff(Rest, Count),
    {<<Head:1, RestDestuffed/bitstring>>, NewCount};

bitdestuff(<<>>, Count) ->
    {<<>>, Count}.


%%
%%  Checksum: see [1], section 4.4.6;
%%  And http://www.billnewhall.com/TechDepot/AX25CRC/CRC_for_AX25.pdf
%%  FCS should not be bit-reversed.
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


%%
%%  Format control byte for an UI frame.
%%
control_byte_ui() ->
    ControlM1 = 2#000,  %% UI frame, see [1], section 4.3.3.
    ControlM2 = 2#00,   %% UI frame, see [1], section 4.3.3.
    ControlPF = 2#0,    %% Not used, see [1], sections 4.3.3, 4.3.3.6, 6.2.
    <<ControlM1:3, ControlPF:1, ControlM2:2, ?CTRL_FRAME_U:2>>.



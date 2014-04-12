%%
%%  [Čia](http://www.pe0sat.vgnet.nl/decoding/satellite-telemetry/sound-card-modem/) rašo,
%%  kad reikia jungtis prie tcp 127.0.0.1 port 8000. Pats soundmodemas ir jo aprašymas yra
%%  [čia](http://uz7.ho.ua/packetradio.htm). Kaip suprantu Soundmodemas naudoja AGW programos
%%  api, tad detaliau pats sąsajos aprašymas yra čia: http://uz7.ho.ua/includes/agwpeapi.htm
%%
%%  Turime susintaliavę ir tą AGW programą ir išbandžiau, bet soundmodemas pasirodė
%%  paprastesnis ir jautresnis.
%%
%%  Geriausi linkėjimai / Best Regards,
%%  Laurynas M
%%
-module(ls1mcs_tnc_agwpe).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-behaviour(ls1mcs_protocol).
-export([start_link/5, send/2, received/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("ls1mcs.hrl").

-define(TIMEOUT, 5000).
-define(TICK, 60000).


%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%  Opts:
%%    * `{user, User}` - AGWPE login
%%    * `{pass, Pass}` - AGWPE login
%%    * `{port, Port = 0}` - AGWPE port
%%    * `{call, Call = NOCALL}` - Our (ground station) call sign
%%    * `{peer, Peer = NOCALL}` - Peer (satellite) call sign.
%%
start_link(Name, ConnHost, ConnPort, Recv, Opts) ->
    Args = {ConnHost, ConnPort, Recv, Opts},
    gen_server:start_link({via, gproc, Name}, ?MODULE, Args, []).


%%
%%
%%
send(Ref, Data) when is_binary(Data) ->
    gen_server:cast({via, gproc, Ref}, {send, Data}).


%%
%%  Not used here.
%%
received(_Ref, _Data) ->
    ok.



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(agwpe_frame, {
    port,
    data_kind,
    call_from,
    call_to,
    data
}).


-record(state, {
    recv,       %% Receiver and its state.
    port,       %% AGWPE port.
    call,       %% Call of the ground station.
    peer,       %% Call of the sattelite.
    user,       %% AGWPE Username.
    pass,       %% AGWPE Password.
    sock,       %% AGWPE TCP/IP Socket.
    buff        %% Incomplete data frame received from the socket.
}).


%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================


%%
%%
%%
init({ConnHost, ConnPort, Recv, Opts}) ->
    self() ! {initialize, ConnHost, ConnPort},
    self() ! {tick},
    Defaults = #{port => 0, call => <<"NOCALL">>, peer => <<"NOCALL">>},
    #{
        port := Port,
        call := Call,
        peer := Peer,
        user := User,
        pass := Pass
    } = maps:merge(Defaults, Opts),
    State = #state{
        recv = Recv,
        port = Port,
        call = Call,
        peer = Peer,
        user = User,
        pass = Pass
    },
    {ok, State}.


%%
%%
%%
handle_call(_Message, _From, State) ->
    {stop, not_implemented, State}.


%%
%%  Send data to the RS232 port.
%%
handle_cast({send, Data}, State = #state{port = Port, call = Call, sock = Sock}) ->
    ok = send_frame(agwpe_send_unproto_info(Port, Call, Data), Sock),
    {noreply, State}.


%%
%%  Deffered initialization.
%%
handle_info({initialize, ConnHost, ConnPort}, State) ->
    timer:sleep(1000), %% Delay for restarts.
    ConnOpts = [binary, {packet, raw}, {active, true}],
    {ok, Sock} = gen_tcp:connect(ConnHost, ConnPort, ConnOpts, ?TIMEOUT),
    NewState = handle_setup(State#state{sock = Sock, buff = <<>>}),
    {noreply, NewState};

%%
%%  Ping the soundmodem periodically.
%%
handle_info({tick}, State = #state{sock = Sock}) ->
    ok = send_frame(agwpe_ask_port_info(), Sock),
    erlang:send_after(?TICK, self(), {tick}),
    {ok, State};

%%
%%  Receive data.
%%
handle_info({tcp, Sock, RecvData}, State = #state{sock = Sock, buff = Buff}) ->
    lager:debug("Received: ~p", [RecvData]),
    {ok, NewState} = handle_received(<<Buff/binary, RecvData/binary>>, State),
    {noreply, NewState}.


%%
%%  Terminate a process.
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
%%  Setup connection.
%%
handle_setup(State = #state{user = User, pass = Pass, sock = Sock}) ->
    ok = send_frame(agwpe_application_login(User, Pass), Sock),
    ok = send_frame(agwpe_enable_raw_frames(), Sock),
    ok = send_frame(agwpe_enable_mon_frames(), Sock),
    {ok, State}.


%%
%%  Process received data (bytes).
%%
handle_received(Received = <<Header:36/binary, Tail/binary>>, State) ->
    {ok, FrameHdr, DataLen} = decode_frame_hdr(Header),
    case DataLen > size(Tail) of
        true ->
            {ok, State#state{buff = Received}};
        false ->
            <<FrameData:DataLen/binary, OtherFrames/binary>> = Tail,
            Frame = FrameHdr#agwpe_frame{data = FrameData},
            {ok, NewState} = handle_frame(Frame, State),
            handle_received(OtherFrames, NewState)
    end;

handle_received(Received, State) ->
    {ok, State#state{buff = Received}}.


%%
%%  Process received frame.
%%
handle_frame(#agwpe_frame{data_kind = $K, data = Data}, State = #state{recv = Recv}) ->
    <<
        Pid:8/unsigned,
        AX25SrcAddr:7/binary,
        AX25DstAddr:7/binary,
        AX25Control:8/unsigned,
        AX25PID:8/unsigned,
        AX25Payload/binary
    >> = Data,
    lager:debug(
        "AGWPE raw unproto (processed): pid=~p, src=~p, dst=~p, ctl=~p, pid=~p, payload=~p",
        [Pid, AX25SrcAddr, AX25DstAddr, AX25Control, AX25PID, AX25Payload]
    ),
    {ok, NewRecv} = ls1mcs_protocol:process(Recv, AX25Payload),
    {ok, State#state{recv = NewRecv}};

handle_frame(#agwpe_frame{data_kind = $U, data = Data}, State) ->
    [Header | _] = binary:split(Data, <<13>>),
    lager:debug("AGWPE unproto (ignored): header=~p.", [Header]),
    {ok, State};

handle_frame(#agwpe_frame{data_kind = $T, data = Data}, State) ->
    lager:debug("AGWPE own unproto (ignored): ~p", [Data]),
    {ok, State};


handle_frame(#agwpe_frame{data_kind = $G, data = Data}, State) ->
    lager:debug("AGWPE port info ~p", [drop_zeros(Data)]),
    {ok, State};

handle_frame(Frame, State) ->
    lager:debug("Ignoring AGWPE frame ~p", [Frame]),
    {ok, State}.


%%
%%  Sends a frame.
%%
send_frame(Frame, Sock) ->
    FrameBin = encode_frame(Frame),
    ok = gen_tcp:send(Sock, FrameBin).


%%
%%  Performs login to the soundmodem.
%%
agwpe_application_login(User, Pass) ->
    EncodedUser = fill_zeros(User, 255),
    EncodedPass = fill_zeros(Pass, 255),
    #agwpe_frame{
        port = 0,
        data_kind = $P,
        call_from = <<>>,
        call_to = <<>>,
        data = <<EncodedUser/binary, EncodedPass/binary>>
    }.


%%
%%  Asks AGWPE for port information.
%%  AGWPE should respond witj "G" command.
%%
agwpe_ask_port_info() ->
    #agwpe_frame{
        port = 0,
        data_kind = $G,
        call_from = <<>>,
        call_to = <<>>,
        data = <<>>
    }.

%%
%%  Switches reception of monitoring frames on or off.
%%
agwpe_enable_mon_frames() ->
    #agwpe_frame{
        port = 0,
        data_kind = $m,
        call_from = <<>>,
        call_to = <<>>,
        data = <<>>
    }.

%%
%%  Activate reception if frames in raw format (in addition to the parsed ones).
%%
agwpe_enable_raw_frames() ->
    #agwpe_frame{
        port = 0,
        data_kind = $k,
        call_from = <<>>,
        call_to = <<>>,
        data = <<>>
    }.

%%
%%  Ask to send unproto frames.
%%
agwpe_send_unproto_info(Port, Call, Data) ->
    #agwpe_frame{
        port = Port,
        data_kind = $M,
        call_from = Call,
        call_to = <<"CQ">>,
        data = Data
    }.



%%
%%  http://uz7.ho.ua/includes/agwpeapi.htm#_Toc500723778
%%
encode_frame(Frame) ->
    #agwpe_frame{
        port = Port,
        data_kind = DataKind,
        call_from = CallFrom,
        call_to = CallTo,
        data = Data
    } = Frame,
    Reserved = 0,
    PID = 0,
    DataLen = size(Data),
    EncodedCallFrom = encode_call(CallFrom),
    EncodedCallTo   = encode_call(CallTo),
    <<
        Port:8/unsigned,
        Reserved:24/unsigned-little,
        DataKind:8/unsigned,
        Reserved:8/unsigned,
        PID:8/unsigned,
        Reserved:8/unsigned,
        EncodedCallFrom/binary,    % 10 bytes
        EncodedCallTo/binary,      % 10 bytes
        DataLen:32/unsigned-little,
        Reserved:32/unsigned-little,
        Data/binary
    >>.


%%
%%
%%
encode_call(Call) ->
    fill_zeros(Call, 10).


%%
%%
%%
decode_frame_hdr(Binary) ->
    <<
        Port:8/unsigned,
        _Reserved1:24/unsigned-little,
        DataKind:8/unsigned,
        _Reserved2:8/unsigned,
        _PID:8/unsigned,
        _Reserved3:8/unsigned,
        CallFrom:10/binary,    % 10 bytes
        CallTo:10/binary,      % 10 bytes
        DataLen:32/unsigned-little,
        _Reserved4:32/unsigned-little
    >> = Binary,
    Frame = #agwpe_frame{
        port        = Port,
        data_kind   = DataKind,
        call_from   = decode_call(CallFrom),
        call_to     = decode_call(CallTo),
        data        = undefined
    },
    {ok, Frame, DataLen}.


%%
%%
%%
decode_call(CallSign) ->
    drop_zeros(CallSign).


%%
%%
%%
fill_zeros(Data, Len) ->
    DataLen = size(Data),
    if
        DataLen =:= Len ->
            Data;
        DataLen =< Len ->
            ZeroBits = (Len - DataLen) * 8,
            <<Data/binary, 0:ZeroBits>>
    end.


%%
%%
%%
drop_zeros(Data) ->
    [WithoutZeros | _] = binary:split(Data, <<0>>),
    WithoutZeros.


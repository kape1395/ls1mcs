%/--------------------------------------------------------------------
%| Copyright 2013-2014 Karolis Petrauskas
%|
%| Licensed under the Apache License, Version 2.0 (the "License");
%| you may not use this file except in compliance with the License.
%| You may obtain a copy of the License at
%|
%|     http://www.apache.org/licenses/LICENSE-2.0
%|
%| Unless required by applicable law or agreed to in writing, software
%| distributed under the License is distributed on an "AS IS" BASIS,
%| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%| See the License for the specific language governing permissions and
%| limitations under the License.
%\--------------------------------------------------------------------

%%
%%  Uses MFJ-1270C TNC.
%%
%%  See:
%%      http://www.repeater-builder.com/mfj/pdfs/mfj-1270c-1274c-tnc-manual.pdf
%%      http://www.ax25.net/kiss.aspx
%%
-module(ls1mcs_tnc_mfj1270c).
-behaviour(ls1mcs_tnc).
-behavour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/6, send/2, invoke/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%%  See ASCII for more details.
%%
-define(CR, 16#0D).

-define(UART_OPTIONS, [{baud, 9600}, {csize, 8}, {parity, none}, {mode, binary}]).
-define(RESTART_DELAY,      1000).   % Milliseconds to sleep before connecting to the UART.
-define(RECV_COUNT,         1).
-define(RECV_TIMEOUT,       1000).
-define(BLOCK_READ_TIMEOUT, 1000).   % Timeout used when reading strings.


%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%  TODO: Check max packet length when sending.
%%
%%  ls1mcs_tnc_mfj1270c:start_link({n, l, test}, {ls1mcs_tnc_mfj1270c, {n, l, test}}, "/dev/ttyUSB0").
%%  ls1mcs_tnc_mfj1270c:send({n, l, test}, <<"Hello world!">>).
%%
start_link(Name, Direction, Device, Password, Call, Peer) ->
    {ok, Pid} = gen_server:start_link({via, gproc, Name}, ?MODULE, {Direction, Device, Password, Call, Peer}, []),
    ok = ls1mcs_tnc:register(?MODULE, Name),
    {ok, Pid}.


%%
%%  Sends data packet via TNC.
%%
send(Name, Frame) ->
    gen_server:cast({via, gproc, Name}, {send, Frame}).


%%
%%  Invokes TNC command in hostmode.
%%  This command can be used for sending commands to the TNC "manually",
%%  Example call:
%%      MFJName = {n, l, ls1mcs_tnc_mfj1270c}.
%%      MFJRef = ls1mcs_protocol:make_ref(ls1mcs_tnc_mfj1270c, HMName).
%%      ls1mcs_tnc_mfj1270c:invoke(HMName, <<"MONITOR">>).
%%      ls1mcs_tnc_mfj1270c:invoke(HMName, <<"CONMODE">>).
%%
invoke(Ref, Command) when is_binary(Command) ->
    gen_server:call({via, gproc, Ref}, {invoke, Command}).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    send,       %% Sending protocol chain.
    recv,       %% Receiving protocol chain.
    device,     %% Device we are working with ("/dev/ttyUSB0")
    port,       %% UART port.
    uplink,     %% Handle uplink.
    downlink    %% Handle downlink.
}).


%% =============================================================================
%%  Callbacks for gen_fsm.
%% =============================================================================


%%
%%  Initialization.
%%
init({Direction, Device, Password, Call, Peer}) ->
    {ok, Ls1pSend} = ls1mcs_proto_ls1p:make_ref(Password, true),
    {ok, Ls1pRecv} = ls1mcs_proto_ls1p:make_ref(Password, true),
    {ok, Ax25Send} = ls1mcs_proto_ax25:make_ref(Call, Peer, tnc),
    {ok, Ax25Recv} = ls1mcs_proto_ax25:make_ref(Call, Peer, tnc),
    {ok, KissSend} = ls1mcs_proto_kiss:make_ref(),
    {ok, KissRecv} = ls1mcs_proto_kiss:make_ref(),
    {ok, SendChain} = ls1mcs_proto:make_send_chain([Ls1pSend, Ax25Send, KissSend]),
    {ok, RecvChain} = ls1mcs_proto:make_recv_chain([KissRecv, Ax25Recv, Ls1pRecv]),
    self() ! {initialize},
    State = #state{
        send     = SendChain,
        recv     = RecvChain,
        device   = Device,
        uplink   = lists:member(Direction, [both, up,   uplink]),
        downlink = lists:member(Direction, [both, down, downlink])
    },
    {ok, State}.


%%
%%  Invoke a command on the TNC synchronously.
%%
handle_call({invoke, Command}, _From, State = #state{port = Port}) ->
    {ok, Response} = send_cmd(Port, Command),
    {reply, Response, State}.


%%
%%  Send data to the TNC.
%%
handle_cast({send, Frame}, State = #state{uplink = false}) ->
    lager:debug("Discarding ~p, uplink disabled.", [Frame]),
    {noreply, State};

handle_cast({send, Frame}, State = #state{send = SendChain, port = Port}) ->
    {ok, BinFrames, NewSendChain} = ls1mcs_proto:send(Frame, SendChain),
    [ ok = uart:send(Port, BinFrame) || BinFrame <- BinFrames ],
    {noreply, State#state{send = NewSendChain}};

handle_cast(_Msg, State) ->
    {stop, undefined, State}.


%%
%%  Initialize COM port and sync with the TNC.
%%
handle_info({initialize}, State = #state{device = Device}) ->
    ok = receive after ?RESTART_DELAY -> ok end,
    %
    {ok, Port} = uart:open(Device, ?UART_OPTIONS),
    case check_kiss_mode(Port) of
        true ->
            lager:debug("Already in KISS mode.");
        false ->
            {ok, _} = send_cmd(Port, <<"CONMODE TRANS">>),
            {ok, _} = send_cmd(Port, <<"KISS ON">>),
            ok = uart:send(Port, <<"RESTART\r">>),
            true = case check_kiss_mode(Port) of
                true ->
                    lager:debug("Entered KISS mode."),
                    true;
                false ->
                    lager:debug("Unable to enter KISS mode."),
                    false
            end
    end,
    self() ! {recv},
    {noreply, State#state{port = Port}};

%%
%%  Receive cycle.
%%
handle_info({recv}, State = #state{port = Port, recv = RecvChain, downlink = Downlink}) ->
    case uart:recv(Port, ?RECV_COUNT, ?RECV_TIMEOUT) of
        {ok, RecvIoList} when Downlink =:= false ->
            lager:debug("Discarding ~p, downlink disabled.", [RecvIoList]),
            self() ! {recv},
            {noreply, State};
        {ok, RecvIoList} ->
            RecvBinary = iolist_to_binary(RecvIoList),
            {ok, RecvFrames, NewRecvChain} = ls1mcs_proto:recv(RecvBinary, RecvChain),
            ok = ls1mcs_sat_link:recv(RecvFrames),
            self() ! {recv},
            {noreply, State#state{recv = NewRecvChain}};
        {error, timeout} ->
            self() ! {recv},
            {noreply, State}
    end.


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
%%  Reads all input available on the link.
%%
read_all(Port) ->
    Input = read_all(Port, []),
    {ok, list_to_binary(Input)}.

read_all(Port, List) ->
    case uart:recv(Port, 1, ?BLOCK_READ_TIMEOUT) of
        {ok, <<Byte:8>>} -> read_all(Port, [Byte | List]);
        {error, timeout} -> lists:reverse(List)
    end.


%%
%%  Sends command to the TNC in the command mode.
%%
send_cmd(Port, Command) when is_binary(Command) ->
    ok = uart:send(Port, <<Command/binary, ?CR>>),
    case read_all(Port) of
        {ok, Response} ->
            case binary:longest_common_suffix([Response, <<"cmd:">>]) of
                4 ->
                    {ok, Response};
                _ ->
                    {error, {badresp, Response}}
            end;
        {error, Error} ->
            {error, Error}
    end.


%%
%%  Tries to determine, if its a KISS mode.
%%
check_kiss_mode(Port) ->
    uart:send(Port, <<16#C00DC0:24>>),
    case read_all(Port) of
        {ok, <<>>} ->
            lager:debug("Check KISS -> true"),
            true;
        {ok, Response} when is_binary(Response) ->
            lager:debug("Check KISS -> false, output=~p", [Response]),
            false
    end.



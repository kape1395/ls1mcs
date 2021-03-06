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
%%  Uses TNC with TAPR EPROM in a KISS mode to send and receive data.
%%  Designed to work with TNC2H-DK9JS.
%%
%%  According to the [TAPR manual](docs/ext/tapr.txt), to enter KISS mode:
%%      uart:send(Port, <<16#C0FFC0:24, 13>>).
%%      uart:send(Port, <<"AWLEN 8", 13>>).
%%      uart:send(Port, <<"PARITY 0", 13>>).
%%      uart:send(Port, <<"RESTART", 13>>).
%%      uart:send(Port, <<"KISS ON", 13>>).
%%      uart:send(Port, <<"RESTART", 13>>).
%%      uart:send(Port, <<16#C00501C0:24>>). % Full duplex
%%      uart:send(Port, <<16#C000:16, 00, 127, 255, 16#C0:8>>). % Send data
%%  To quit the KISS mode:
%%      <<16#C0FFC0:24>>
%%
%%
%%  {ok, Port} = uart:open("/dev/ttyUSB0", [{baud, 9600}, {csize, 7}, {parity, even}, {mode, binary}]).
%%
-module(ls1mcs_tnc_tapr_kiss).
-behaviour(ls1mcs_tnc).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/6, send/2, reenter_kiss_mode/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("ls1mcs.hrl").

-define(RECV_COUNT,   1).
-define(RECV_TIMEOUT, 1000).
-define(RESTART_DELAY, 1000).
-define(UART_OPTIONS, [{baud, 9600}, {csize, 8}, {parity, none}, {mode, binary}, {exit_on_close, true}]).
-define(BLOCK_READ_TIMEOUT, 1000).   % Timeout used when reading strings.


%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%
%%
start_link(Name, Direction, Device, Password, Call, Peer) ->
    {ok, Pid} = gen_server:start_link({via, gproc, Name}, ?MODULE, {Name, Direction, Device, Password, Call, Peer}, []),
    {ok, Pid}.


%%
%%
%%
send(Name, Frame) ->
    gen_server:cast({via, gproc, Name}, {send, Frame}).


%%
%%  Re-enters KISS mode.
%%
reenter_kiss_mode(Ref) ->
    gen_server:call({via, gproc, Ref}, {reenter_kiss_mode}, 30000).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    send,       %% Sending protocol chain.
    recv,       %% Receiving protocol chain.
    port,       %% UART port.
    uplink,     %% Handle uplink.
    downlink    %% Handle downlink.
}).


%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================


%%
%%
%%
init({Name, Direction, Device, Password, Call, Peer}) ->
    {ok, Ls1pSend} = ls1mcs_proto_ls1p:make_ref(Password, true),
    {ok, Ls1pRecv} = ls1mcs_proto_ls1p:make_ref(Password, true),
    {ok, Ax25Send} = ls1mcs_proto_ax25:make_ref(Call, Peer, tnc),
    {ok, Ax25Recv} = ls1mcs_proto_ax25:make_ref(Call, Peer, tnc),
    {ok, KissSend} = ls1mcs_proto_kiss:make_ref(),
    {ok, KissRecv} = ls1mcs_proto_kiss:make_ref(),
    {ok, SendChain} = ls1mcs_proto:make_send_chain([Ls1pSend, Ax25Send, KissSend]),
    {ok, RecvChain} = ls1mcs_proto:make_recv_chain([KissRecv, Ax25Recv, Ls1pRecv]),
    self() ! {initialize, Device},
    State = #state{
        send     = SendChain,
        recv     = RecvChain,
        uplink   = lists:member(Direction, [both, up,   uplink]),
        downlink = lists:member(Direction, [both, down, downlink])
    },
    ok = ls1mcs_tnc:register(?MODULE, Name),
    {ok, State}.


%%
%%
%%
handle_call({reenter_kiss_mode}, _From, State = #state{port = Port}) ->
    WasKissMode = check_kiss_mode(Port),
    ok = exit_kiss_mode(Port),
    ok = enter_kiss_mode(Port),
    true = check_kiss_mode(Port),
    {reply, {ok, WasKissMode}, State}.


%%
%%  Send frame to the RS232 port.
%%
handle_cast({send, Frame}, State = #state{uplink = false}) ->
    lager:debug("Discarding ~p, uplink disabled.", [Frame]),
    {noreply, State};

handle_cast({send, Frame}, State = #state{send = SendChain, port = Port}) ->
    {ok, BinFrames, NewSendChain} = ls1mcs_proto:send(Frame, SendChain),
    [ ok = uart:send(Port, BinFrame) || BinFrame <- BinFrames ],
    {noreply, State#state{send = NewSendChain}}.


%%
%%  Deffered initialization.
%%
handle_info({initialize, Device}, State) ->
    ok = receive after ?RESTART_DELAY -> ok end,
    {ok, Port} = uart:open(Device, ?UART_OPTIONS),
    case check_kiss_mode(Port) of
        true ->
            ok = exit_kiss_mode(Port),
            ok = enter_kiss_mode(Port),
            true = check_kiss_mode(Port),
            lager:info("Successfully re-entered KISS mode.");
        false ->
            ok = enter_kiss_mode(Port),
            true = check_kiss_mode(Port),
            lager:info("Successfully entered KISS mode.")
    end,
    self() ! {recv},
    {noreply, State#state{port = Port}};

%%
%%  Receive cycle.
%%
handle_info({recv}, State = #state{recv = RecvChain, port = Port, downlink = Downlink}) ->
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
    end;

%%
%%  Handle listener restarts.
%%
handle_info({gen_event_EXIT, _Handler, Reason}, State) ->
    {stop, {sll_exit, Reason}, State}.


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


%%
%%  Exits KISS mode.
%%
exit_kiss_mode(Port) ->
    %%  Exit KISS mode.
    uart:send(Port, <<16#C0FFC0:24>>),
    {ok, Resp} = read_all(Port),
    lager:debug("Exited KISS mode, output: ~p", [Resp]),

    %%  Assert, if its command mode.
    uart:send(Port, <<"CSTATUS", 13>>),
    {ok, CStatusResp} = read_all(Port),
    lager:debug("CSTATUS responded with: ~p", [CStatusResp]),
    case binary:match(CStatusResp, <<"Link state is">>) of
        X when is_list(X) -> ok;
        X when is_tuple(X) -> ok
    end.


%%
%%  (Re)enters KISS mode.
%%
enter_kiss_mode(Port) ->
    %%  Clear the buffer.
    {ok, InitialResp} = read_all(Port),
    lager:debug("Reentering KISS mode. Initial output: ~p", [InitialResp]),

    %% Exit KISS mode, if it was ON. 13 is for the case of non-KISS mode.
    uart:send(Port, <<16#C0FFC0:24, 13>>),
    {ok, QuitKISSResp} = read_all(Port),
    lager:debug("Tried to switch to the normal mode. Output: ~p", [QuitKISSResp]),
    true = size(QuitKISSResp) > 0,

    %%  Set word length and parity.
    uart:send(Port, <<"AWLEN 8", 13>>),         %% Set word size to 8.
    uart:send(Port, <<"PARITY 0", 13>>),        %% Set parity to none.
    uart:send(Port, <<"RESTART", 13>>),
    {ok, ConfigureResp} = read_all(Port),
    lager:debug("Configured word length and parity. Output: ~p", [ConfigureResp]),
    true = size(QuitKISSResp) > 0,

    %%  Enter KISS mode and set full-duplex ON.
    uart:send(Port, <<"KISS ON", 13>>),         %% Enter KISS mode.
    uart:send(Port, <<"RESTART", 13>>),         %% Needed to enter KISS mode.
    uart:send(Port, <<16#C00501C0:24>>),        %% Set full duplex
    {ok, EnterKISSResp} = read_all(Port),
    lager:debug("Entered KISS mode. Output: ~p", [EnterKISSResp]),
    ok.


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



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
%%  Uses KISS mode SoundModem to send and receive data.
%%
-module(ls1mcs_tnc_smodem).
-behaviour(ls1mcs_tnc).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/6, send/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("ls1mcs.hrl").

-define(RECV_COUNT,   1).
-define(RECV_TIMEOUT, 1000).


%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%
%%
start_link(Name, Direction, Device, Password, Call, Peer) ->
    {ok, Pid} = gen_server:start_link({via, gproc, Name}, ?MODULE, {Direction, Device, Password, Call, Peer}, []),
    ok = ls1mcs_tnc:register(?MODULE, Name),
    {ok, Pid}.


%%
%%
%%
send(Name, Frame) ->
    gen_server:cast({via, gproc, Name}, {send, Frame}).



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
init({Direction, Device, Password, Call, Peer}) ->
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
    {ok, State}.


%%
%%
%%
handle_call(_Message, _From, State) ->
    {stop, not_implemented, State}.


%%
%%  Send data to the RS232 port.
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
    ok = receive after 1000 -> ok end,  %% Delay for restarts.
    {ok, Port} = uart:open(Device, []),
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
            lager:debug("Received: ~p", [RecvIoList]),
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


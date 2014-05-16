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
%%  Uses TNC with WA8DED EPROM (The Firmware 2.6) in a host mode to
%%  send and receive data. Designed to work with TNC2H-DK9JS.
%%  Tested with DIP switches: 1, 3 up and all other down.
%%
%%  See `http://www.ir3ip.net/iw3fqg/doc/wa8ded.htm` for more details.
%%
-module(ls1mcs_tnc_wa8ded_hm).
-behaviour(ls1mcs_tnc).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/5, send/2, invoke/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%%  See ASCII for more details.
%%
-define(DC1,    16#11).
-define(CAN,    16#18).
-define(ESC,    16#1B).
-define(CR,     16#0D).
-define(SPACE,  16#20).

-define(HM_CHANNEL0, 0).
-define(HM_CHANNEL1, 1).
-define(HM_INFO, 0).
-define(HM_CMD, 1).
-define(HM_CODE_OK,         0). % Success, nothing follows  short format
-define(HM_CODE_OK_MSG,     1). % Success, message follows  null-terminated format
-define(HM_CODE_FAILURE,    2). % Failure, message follows  null-terminated format
-define(HM_CODE_STATUS,     3). % Link status               null-terminated format
-define(HM_CODE_MHDR,       4). % Monitor header/no info    null-terminated format
-define(HM_CODE_MHDR_MSG,   5). % Monitor header/info       null-terminated format
-define(HM_CODE_MON_INFO,   6). % Monitor information       byte-count format
-define(HM_CODE_CON_INFO,   7). % Connected information     byte-count format

-define(UART_OPTIONS, [{baud, 9600}, {csize, 8}, {parity, none}, {mode, binary}]).
-define(RESTART_DELAY,      1000).   % Milliseconds to sleep before connecting to the UART.
-define(QUERY_DELAY,        1000).   % Milliseconds.
-define(RESYNC_ATTEMPTS,     300).   % Just because 300 > 256.
-define(RESYNC_DELAY,        100).   % Time to wait for response when performing HM sync.
-define(BLOCK_READ_TIMEOUT, 1000).   % Timeout used when reading strings.
-define(COMMAND_TIMEOUT,    5000).   % Time to wait for a command response.



%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%  TODO: Check max packet length when sending.
%%  TODO: Add some TNC commands to this module. e.g. quit hostmode, reset.
%%
%%  ls1mcs_tnc_wa8ded_hm:start_link({n, l, test}, {ls1mcs_tnc_wa8ded_hm, {n, l, test}}, "/dev/ttyUSB0", "LY2EN").
%%  ls1mcs_tnc_wa8ded_hm:send({n, l, test}, <<"Hello world!">>).
%%
start_link(Name, Direction, Device, Password, Call) ->
    {ok, Pid} = gen_server:start_link({via, gproc, Name}, ?MODULE, {Direction, Device, Password, Call}, []),
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
%%      HMName = {n, l, ls1mcs_tnc_wa8ded_hm}.
%%      HMRef = ls1mcs_protocol:make_ref(ls1mcs_tnc_wa8ded_hm, HMName).
%%      ls1mcs_tnc_wa8ded_hm:invoke(HMName, <<"@B">>).    % Show free memory
%%      ls1mcs_tnc_wa8ded_hm:invoke(HMName, <<"@D 1">>).  % Full duplex ON
%%
invoke(Ref, Command) when is_binary(Command) ->
    gen_server:call({via, gproc, Ref}, {invoke, Command}).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    send,       %% Sending protocol chain.
    recv,       %% Receiving protocol chain.
    device,     %% Device we are working with (/dev/ttyUSB0)
    local_call, %% Local call (HAM)
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
init({Direction, Device, Password, Call}) ->
    {ok, Ls1pSend} = ls1mcs_proto_ls1p:make_ref(Password, true),
    {ok, Ls1pRecv} = ls1mcs_proto_ls1p:make_ref(Password, true),
    {ok, SendChain} = ls1mcs_proto:make_send_chain([Ls1pSend]),
    {ok, RecvChain} = ls1mcs_proto:make_recv_chain([Ls1pRecv]),
    self() ! {initialize},
    State = #state{
        send       = SendChain,
        recv       = RecvChain,
        device     = Device,
        local_call = Call,
        uplink     = lists:member(Direction, [both, up,   uplink]),
        downlink   = lists:member(Direction, [both, down, downlink])
    },
    {ok, State}.


%%
%%  Invoke a command on the TNC synchronously.
%%
handle_call({invoke, Command}, _From, State = #state{port = Port}) ->
    Response = invoke_cmd(Port, Command),
    {reply, Response, State}.


%%
%%  Send data to the TNC.
%%
handle_cast({send, Frame}, State = #state{uplink = false}) ->
    lager:debug("Discarding ~p, uplink disabled.", [Frame]),
    {noreply, State};

handle_cast({send, Frame}, State = #state{send = SendChain, port = Port}) ->
    {ok, BinFrames, NewSendChain} = ls1mcs_proto:send(Frame, SendChain),
    [ ok = send_info(Port, BinFrame) || BinFrame <- BinFrames ],
    {noreply, State#state{send = NewSendChain}};

handle_cast(_Msg, State) ->
    {stop, undefined, State}.


%%
%%  Initialize COM port and sync with the TNC.
%%
handle_info({initialize}, State = #state{device = Device, local_call = LocalCall}) ->
    ok = receive after ?RESTART_DELAY -> ok end,
    {ok, Port} = uart:open(Device, ?UART_OPTIONS),
    ok = enter_hostmode(Port),
    {ok} = invoke_cmd(Port, <<"M U">>),    % Monitor UI frames.
    {ok} = invoke_cmd(Port, <<"E O">>),    % Echo OFF.
    ok = set_local_call(Port, LocalCall),

    self() ! {query_input},
    {noreply, State#state{port = Port}};

%%
%%  Check, if incoming messages arrived.
%%
handle_info({query_input}, State = #state{recv = RecvChain, port = Port, downlink = Downlink}) ->
    {ok, NewRecvChain} = query_all_input(Port, RecvChain, Downlink),
    _TRef = erlang:send_after(?QUERY_DELAY, self(), {query_input}),
    {noreply, State#state{recv = NewRecvChain}}.


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
%%  Reads input till zero-byte.
%%
read_zstr(Port) ->
    Input = read_zstr(Port, []),
    {ok, list_to_binary(Input)}.

read_zstr(Port, List) ->
    case uart:recv(Port, 1, ?BLOCK_READ_TIMEOUT) of
        {ok, <<0:8>>} -> lists:reverse(List);
        {ok, <<Byte:8>>} -> read_zstr(Port, [Byte | List]);
        {error, timeout} -> {error, timeout}
    end.


%%
%%  Reads length-prefixed data.
%%
read_lstr(Port) ->
    case uart:recv(Port, 1, ?BLOCK_READ_TIMEOUT) of
        {ok, <<ByteCount:8>>} ->
            case uart:recv(Port, ByteCount + 1, ?BLOCK_READ_TIMEOUT) of
                {ok, Message} -> {ok, Message};
                {error, timeout} -> {error, timeout}
            end;
        {error, timeout} ->
            {error, timeout}
    end.


%%
%%  Executes hostmode resync procedure: sends 1's till error is received.
%%
resync_hostmode(Port) ->
    resync_hostmode(Port, ?RESYNC_ATTEMPTS).

resync_hostmode(_Port, 0) ->
    {error, unable_to_sync};

resync_hostmode(Port, Count) when Count > 0 ->
    ok = uart:send(Port, <<1>>),
    case uart:recv(Port, 1, ?RESYNC_DELAY) of
        {ok, <<?HM_CHANNEL1>>} ->
            % Assume <<1>> is channel number. Now we expect of <<2>> -- error indicator.
            % If <<2>> received, read zero-ended error message and assume sync'ed state.
            case uart:recv(Port, 1, ?RESYNC_DELAY) of
                {ok, <<?HM_CODE_FAILURE>>} ->
                    {ok, ErrMsg} = read_zstr(Port),
                    lager:debug("Expexted error ~p received, synced!", [ErrMsg]),
                    ok;
                {ok, OtherCode} ->
                    {ok, OtherTrash} = read_all(Port),
                    lager:debug("Got ~p instead of error indicator and trash following it: ~p.", [OtherCode, OtherTrash]),
                    resync_hostmode(Port, Count - 1);
                {error, timeout} ->
                    lager:debug("Do we have echo here?"),
                    resync_hostmode(Port, Count - 1)
            end;
        {ok, OtherMsg} ->
            lager:debug("Got ~p instead of error message on channel 1.", [OtherMsg]),
            {ok, _} = read_all(Port),
            resync_hostmode(Port, Count - 1);
        {error, timeout} ->
            resync_hostmode(Port, Count - 1)
    end.


%%
%%  Sends command to the Unproto channel (0) and reads its return code.
%%
send_cmd(Port, Command) when is_binary(Command) ->
    Count = size(Command) - 1,
    ok = uart:send(Port, <<?HM_CHANNEL0, ?HM_CMD, Count, Command/binary>>),
    {ok, <<?HM_CHANNEL0, Code>>} = uart:recv(Port, 2, ?COMMAND_TIMEOUT),
    {ok, Code}.


%%
%%  Send info via Unproto channel (0).
%%  This function asserts success response.
%%
send_info(Port, Data) when is_binary(Data) ->
    Count = size(Data) - 1,
    ok = uart:send(Port, <<?HM_CHANNEL0, ?HM_INFO, Count, Data/binary>>),
    {ok, <<?HM_CHANNEL0, ?HM_CODE_OK>>} = uart:recv(Port, 2, ?COMMAND_TIMEOUT),
    ok.


%%
%%  1. Cleanup input.
%%  2. Try to switch from terminal-mode to command-mode.
%%  3. Try to resync in hostmode.
%%
determine_mode(Port) ->
    lager:debug("Determining TNC mode..."),

    {ok, Trash} = read_all(Port),
    lager:debug("Trash read from the TNC: ~p.", [Trash]),

    ok = uart:send(Port, <<?DC1, ?CAN, ?ESC>>),
    {ok, EscResponse} = read_all(Port),
    lager:debug("Command mode request responded with: ~p.", [EscResponse]),

    case lists:reverse(binary_to_list(EscResponse)) of
        [?SPACE, $* | _] ->
            lager:debug("Determined, its terminal mode!"),
            {ok, terminal};
        _ ->
            lager:debug("Its not terminal mode? Trying to resync hostmode."),
            case resync_hostmode(Port) of
                ok -> {ok, hostmode};
                {error, _} -> {error, unknown_mode}
            end
    end.


%%
%%  Enter host-mode and resync.
%%
enter_hostmode(Port) ->
    case determine_mode(Port) of
        {ok, hostmode} ->
            lager:info("TNC already in hostmode, synchronization done."),
            ok;
        {ok, terminal} ->
            lager:info("TNC is in terminal mode, switching to hostmode..."),
            ok = uart:send(Port, <<?DC1, ?CAN, ?ESC, "JHOST 1", ?CR>>),
            {ok, hostmode} = determine_mode(Port),
            lager:info("TNC switched to hostmode and synchronized."),
            ok
    end.


%%
%%  Fetches all pending incoming messages and
%%  sends them to the upper protocol layer.
%%
query_all_input(Port, RecvChain, Downlink) ->
    case send_cmd(Port, <<"G">>) of
        {ok, ?HM_CODE_OK} ->
            % OK, no data available on the link.
            {ok, RecvChain};
        {ok, ?HM_CODE_OK_MSG} ->
            {ok, Msg} = read_zstr(Port),
            lager:warning("G responded OK with message ~p", [Msg]),
            {ok, RecvChain};
        {ok, ?HM_CODE_FAILURE} ->
            {ok, Msg} = read_zstr(Port),
            lager:error("G failed with message ~p", [Msg]),
            {error, {failure, Msg}};
        {ok, ?HM_CODE_STATUS} ->
            {ok, Msg} = read_zstr(Port),
            lager:info("G responded with status message ~p", [Msg]),
            query_all_input(Port, RecvChain, Downlink);
        {ok, ?HM_CODE_MHDR} ->
            {ok, Msg} = read_zstr(Port),
            lager:debug("G responded with monitored header ~p", [Msg]),
            query_all_input(Port, RecvChain, Downlink);
        {ok, ?HM_CODE_MHDR_MSG} ->
            {ok, Msg} = read_zstr(Port),
            lager:debug("G responded with monitored header ~p, info follows", [Msg]),
            ok = query_info(Port, RecvChain, Downlink),
            query_all_input(Port, RecvChain, Downlink)
    end.

query_info(Port, RecvChain, Downlink) ->
    case send_cmd(Port, <<"G">>) of
        {ok, ?HM_CODE_MON_INFO} ->
            {ok, Message} = read_lstr(Port),
            case Downlink of
                true ->
                    lager:debug("Monitored info received: ~p", [Message]),
                    {ok, RecvFrames, NewRecvChain} = ls1mcs_proto:recv(Message, RecvChain),
                    ok = ls1mcs_sat_link:recv(RecvFrames),
                    {ok, NewRecvChain};
                false ->
                    lager:debug("Discarding monitored info ~p, downlink disabled.", [Message]),
                    {ok, RecvChain}
            end;
        {ok, ?HM_CODE_CON_INFO} ->
            {ok, Message} = read_lstr(Port),
            case Downlink of
                true ->
                    lager:warn("Connected info received: ~p", [Message]),
                    {ok, RecvFrames, NewRecvChain} = ls1mcs_proto:recv(Message, RecvChain),
                    ok = ls1mcs_sat_link:recv(RecvFrames),
                    {ok, NewRecvChain};
                false ->
                    lager:debug("Discarding connected info ~p, downlink disabled.", [Message]),
                    {ok, RecvChain}
            end
    end.


%%
%%  Set local call for the TNC.
%%
set_local_call(Port, Call) ->
    CallBin = list_to_binary(Call),
    case send_cmd(Port, <<"I ", CallBin/binary>>) of
        {ok, ?HM_CODE_OK} ->
            lager:info("Local call set to ~p", [CallBin]),
            ok;
        {ok, ?HM_CODE_OK_MSG} ->
            {ok, Msg} = read_zstr(Port),
            lager:info("Local call set to ~p, message=~p", [CallBin, Msg]),
            ok;
        {ok, ?HM_CODE_FAILURE} ->
            {ok, Msg} = read_zstr(Port),
            lager:error("Unable to set call to ~p, error=~p", [CallBin, Msg]),
            {error, {failure, Msg}}
    end.


%%
%%  Invoke TNC command and get its output.
%%
invoke_cmd(Port, Command) ->
    case send_cmd(Port, Command) of
        {ok, ?HM_CODE_OK} ->
            {ok};
        {ok, ?HM_CODE_OK_MSG} ->
            {ok, _Msg} = read_zstr(Port);
        {ok, ?HM_CODE_FAILURE} ->
            {ok, Msg} = read_zstr(Port),
            {failure, Msg}
    end.



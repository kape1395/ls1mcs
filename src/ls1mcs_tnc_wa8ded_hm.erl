%%
%%  Uses TNC with WA8DED EPROM (The Firmware 2.6) in a host mode to
%%  send and receive data. Designed to work with TNC2H-DK9JS.
%%  Tested with DIP switches: 1, 3 up and all other down.
%%
%%  See `http://www.ir3ip.net/iw3fqg/doc/wa8ded.htm` for more details.
%%
-module(ls1mcs_tnc_wa8ded_hm).
-compile([{parse_transform, lager_transform}]).
-behavour(gen_server).
-behaviour(ls1mcs_protocol).
-export([start_link/4, send/2, received/2]). % Public API
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
-define(RESTART_DELAY, 1000).       % Milliseconds to sleep before connecting to the UART.
-define(QUERY_DELAY, 1000).         % Milliseconds.
-define(RESYNC_ATTEMPTS, 300).      % Just because 300 > 256.
-define(RESYNC_DELAY, 100).         % Time to wait for response when performing HM sync.
-define(BLOCK_READ_TIMEOUT, 1000).  % Timeout used when reading strings.
-define(COMMAND_TIMEOUT, 5000).     % Time to wait for a command response.



%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%  TODO: Check max packet length when sending.
%%  TODO: Add some TNC commands to this module. e.g. quit hostmode, reset.
%%  TODO: Set own call.
%%
%%  ls1mcs_tnc_wa8ded_hm:start_link({n, l, test}, {ls1mcs_tnc_wa8ded_hm, {n, l, test}}, "/dev/ttyUSB0", "LY2EN").
%%  ls1mcs_tnc_wa8ded_hm:send({n, l, test}, <<"Hello world!">>).
%%
start_link(Name, Upper, Device, LocalCall) ->
    gen_server:start_link({via, gproc, Name}, ?MODULE, {Upper, Device, LocalCall}, []).


%%
%%
%%
send(Ref, Data) when is_binary(Data) ->
    gen_server:cast({via, gproc, Ref}, {send, Data}).


%%
%%
%%
received(_Ref, Data) ->
    lager:error("received/2 is not supported in this module, but ~p received.", [Data]),
    undefined.



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    upper,      %% Upper protocol layer.
    device,     %% Device we are working with (/dev/ttyUSB0)
    local_call, %% Local call (HAM)
    port        %% UART port.
}).


%% =============================================================================
%%  Callbacks for gen_fsm.
%% =============================================================================


%%
%%
%%
init({Upper, Device, LocalCall}) ->
    self() ! {initialize},
    {ok, #state{upper = Upper, device = Device, local_call = LocalCall}}.


%%
%%
%%
handle_call(_Msg, _From, State) ->
    {stop, undefined, State}.


%%
%%  Send data to the TNC.
%%
handle_cast({send, Data}, State = #state{port = Port}) ->
    ok = send_info(Port, Data),
    {noreply, State};

handle_cast(_Msg, State) ->
    {stop, undefined, State}.


%%
%%  Initialize COM port and sync with the TNC.
%%
handle_info({initialize}, State = #state{upper = Upper, device = Device, local_call = LocalCall}) ->
    ls1mcs_protocol:await(Upper),
    lager:debug("Upper protocol started"),

    ok = receive after ?RESTART_DELAY -> ok end,
    {ok, Port} = uart:open(Device, ?UART_OPTIONS),
    ok = enter_hostmode(Port),
    ok = set_local_call(Port, LocalCall),

    self() ! {query_input},
    {noreply, State#state{port = Port}};

%%
%%  Check, if incoming messages arrived.
%%
handle_info({query_input}, State = #state{upper = Upper, port = Port}) ->
    ok = query_all_input(Port, Upper),
    _TRef = erlang:send_after(?QUERY_DELAY, self(), {query_input}),
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
            case uart:recv(Port, ByteCount, ?BLOCK_READ_TIMEOUT) of
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
                OtherCode ->
                    {ok, OtherTrash} = read_all(Port),
                    lager:debug("Got ~p instead of error indicator and trash following it: ~p.", [OtherCode, OtherTrash]),
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
query_all_input(Port, Upper) ->
    case send_cmd(Port, <<"G">>) of
        {ok, ?HM_CODE_OK} ->
            % OK, no data available on the link.
            ok;
        {ok, ?HM_CODE_OK_MSG} ->
            {ok, Msg} = read_zstr(Port),
            lager:warning("G responded OK with message ~p", [Msg]),
            ok;
        {ok, ?HM_CODE_FAILURE} ->
            {ok, Msg} = read_zstr(Port),
            lager:error("G failed with message ~p", [Msg]),
            {error, {failure, Msg}};
        {ok, ?HM_CODE_STATUS} ->
            {ok, Msg} = read_zstr(Port),
            lager:info("G responded with status message ~p", [Msg]),
            query_all_input(Port, Upper);
        {ok, ?HM_CODE_MHDR} ->
            {ok, Msg} = read_zstr(Port),
            lager:debug("G responded with monitored header ~p", [Msg]),
            query_all_input(Port, Upper);
        {ok, ?HM_CODE_MHDR_MSG} ->
            {ok, Msg} = read_zstr(Port),
            lager:debug("G responded with monitored header ~p, info follows", [Msg]),
            query_info(Port, Upper)
    end.

query_info(Port, Upper) ->
    case send_cmd(Port, <<"G">>) of
        {ok, ?HM_CODE_MON_INFO} ->
            {ok, Message} = read_lstr(Port),
            lager:debug("Monitored info received: ~p", [Message]),
            ok = ls1mcs_protocol:received(Upper, Message);
        {ok, ?HM_CODE_CON_INFO} ->
            {ok, Message} = read_lstr(Port),
            lager:warn("Connected info received: ~p", [Message]),
            ok = ls1mcs_protocol:received(Upper, Message)
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





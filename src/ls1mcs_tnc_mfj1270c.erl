%%
%%  Uses MFJ-1270C TNC.
%%
%%  See:
%%      http://www.repeater-builder.com/mfj/pdfs/mfj-1270c-1274c-tnc-manual.pdf
%%      http://www.ax25.net/kiss.aspx
%%
-module(ls1mcs_tnc_mfj1270c).
-behavour(gen_server).
-behaviour(ls1mcs_protocol).
-compile([{parse_transform, lager_transform}]).
-export([start_link/3, send/2, received/2, invoke/2]).
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
start_link(Name, Upper, Device) ->
    gen_server:start_link({via, gproc, Name}, ?MODULE, {Upper, Device}, []).


%%
%%  Sends data packet via TNC.
%%
send(Ref, Data) when is_binary(Data) ->
    gen_server:cast({via, gproc, Ref}, {send, Data}).


%%
%%  Not used.
%%
received(_Ref, Data) ->
    lager:error("received/2 is not supported in this module, but ~p received.", [Data]),
    undefined.


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
    upper,      %% Upper protocol layer.
    device,     %% Device we are working with ("/dev/ttyUSB0")
    port        %% UART port.
}).


%% =============================================================================
%%  Callbacks for gen_fsm.
%% =============================================================================


%%
%%  Initialization.
%%
init({Upper, Device}) ->
    self() ! {initialize},
    {ok, #state{upper = Upper, device = Device}}.


%%
%%  Invoke a command on the TNC synchronously.
%%
handle_call({invoke, Command}, _From, State = #state{port = Port}) ->
    {ok, Response} = send_cmd(Port, Command),
    {reply, Response, State}.


%%
%%  Send data to the TNC.
%%
handle_cast({send, Data}, State = #state{port = Port}) ->
    ok = uart:send(Port, Data),
    {noreply, State};

handle_cast(_Msg, State) ->
    {stop, undefined, State}.


%%
%%  Initialize COM port and sync with the TNC.
%%
handle_info({initialize}, State = #state{upper = Upper, device = Device}) ->
    ls1mcs_protocol:await(Upper),
    lager:debug("Upper protocol started"),
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
handle_info({recv}, State = #state{port = Port, upper = Upper}) ->
    case uart:recv(Port, ?RECV_COUNT, ?RECV_TIMEOUT) of
        {ok, RecvIoList} ->
            RecvBinary = iolist_to_binary(RecvIoList),
            ok = ls1mcs_protocol:received(Upper, RecvBinary),
            self() ! {recv};
        {error, timeout} ->
            self() ! {recv}
    end,
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



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
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-behaviour(ls1mcs_protocol).
-export([start_link/3, send/2, received/2, reenter_kiss_mode/1]).
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
start_link(Name, Upper, Device) ->
    gen_server:start_link({via, gproc, Name}, ?MODULE, {Upper, Device}, []).


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


%%
%%  Re-enters KISS mode.
%%
reenter_kiss_mode(Ref) ->
    gen_server:call({via, gproc, Ref}, {reenter_kiss_mode}, 30000).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    upper,      %% Upper protocol ref.
    port        %% UART port.
}).


%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================


%%
%%
%%
init({Upper, Device}) ->
    self() ! {initialize, Device},
    {ok, #state{upper = Upper}}.


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
%%  Send data to the RS232 port.
%%
handle_cast({send, Data}, State = #state{port = Port}) ->
    ok = uart:send(Port, Data),
    {noreply, State}.


%%
%%  Deffered initialization.
%%
handle_info({initialize, Device}, State = #state{upper = Upper}) ->
    ls1mcs_protocol:await(Upper),
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



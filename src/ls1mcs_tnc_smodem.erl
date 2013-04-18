%%
%%  Uses KISS mode SoundModem to send and receive data.
%%
-module(ls1mcs_tnc_smodem).
-behaviour(gen_server).
-behaviour(ls1mcs_protocol).
-export([start_link/3, send/2, received/2]).
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
handle_call(_Message, _From, State) ->
    {stop, not_implemented, State}.


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
    ok = receive after 1000 -> ok end,  %% Delay for restarts.
    {ok, Port} = uart:open(Device, []),
    self() ! {recv},
    {noreply, State#state{port = Port}};

%%
%%  Receive cycle.
%%
handle_info({recv}, State = #state{port = Port, upper = Upper}) ->
    case uart:recv(Port, ?RECV_COUNT, ?RECV_TIMEOUT) of
        {ok, RecvIoList} ->
            RecvBinary = iolist_to_binary(RecvIoList),
            ok = ls1mcs_protocol:received(Upper, RecvBinary);
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


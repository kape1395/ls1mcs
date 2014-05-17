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
%%  Uart logger, was used for testing Helium-100 command interface.
%%
-module(ls1mcs_utl_uart_logger).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/1, send/1, recv/0, recv/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(RECV_COUNT,   1).
-define(RECV_TIMEOUT, 1000).


%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%
%%
start_link(Device) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Device}, []).


%%
%%  Send data to UART.
%%
send(Data) when is_binary(Data) ->
    gen_server:call(?MODULE, {send, Data}).


%%
%%  Get data received from UART.
%%
recv() ->
    recv(false).

recv(Clear) ->
    gen_server:call(?MODULE, {recv, Clear}).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    data = [],  %%  Data, received from UART.
    port        %%  UART port.
}).


%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================


%%
%%
%%
init({Device}) ->
    self() ! {init, Device},
    {ok, #state{}}.


%%
%%
%%
handle_call({send, Data}, _From, State = #state{port = Port}) ->
    ok = uart:send(Port, Data),
    {reply, ok, State};

handle_call({recv, Clear}, _From, State = #state{data = Data}) ->
    NewState = case Clear of
        true ->
            State#state{data = []};
        false ->
            State
    end,
    {reply, {ok, erlang:iolist_to_binary(lists:reverse(Data))}, NewState}.


%%
%%
%%
handle_cast(_Message, State) ->
    {noreply, State}.


%%
%%  Deffered initialization.
%%
handle_info({init, Device}, State) ->
    {ok, Port} = uart:open(Device, []),
    self() ! {recv},
    {noreply, State#state{port = Port}};

%%
%%  Receive cycle.
%%
handle_info({recv}, State = #state{port = Port, data = Data}) ->
    self() ! {recv},
    case uart:recv(Port, ?RECV_COUNT, ?RECV_TIMEOUT) of
        {ok, RecvIoList} ->
            RecvBinary = iolist_to_binary(RecvIoList),
            lager:debug("Logger received: ~s (~p)", [hex(RecvBinary), RecvIoList]),
            {noreply, State#state{data = [RecvBinary | Data]}};
        {error, timeout} ->
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

%%
%%
%%
hex(Binary) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Binary)]).


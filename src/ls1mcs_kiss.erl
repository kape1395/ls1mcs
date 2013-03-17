%%
%%  Implementation of the KISS protocol.
%%  See http://www.ax25.net/kiss.aspx for more details.
%%
-module(ls1mcs_kiss).
-behaviour(gen_fsm).
-export([start_link/2, encode/1, decode/2]). % Public API
-export([idle/2, frame_start/2, frame_data/2, frame_esc/2]). %% FSM States
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(FEND,   16#C0).
-define(FESC,   16#DB).
-define(TFEND,  16#DC).
-define(TFESC,  16#DD).
-define(FT_DATA,        0). % Frame type = data frame, port number =  0.
-define(FT_TXDELAY,     1).
-define(FT_P,           2).
-define(FT_SlotTime,    3).
-define(FT_TXtail,      4).
-define(FT_FullDuplex,  5).
-define(FT_SetHardware, 6).


%% =============================================================================
%%  Public API
%% =============================================================================

start_link(Name, Receiver) ->
    gen_fsm:start_link(Name, ?MODULE, {Receiver}, []).


encode(Data) when is_binary(Data) ->
    list_to_binary(encode(binary_to_list(Data)));

encode(Data) when is_list(Data) ->
    EncodedData = [?FEND, ?FT_DATA, lists:map(fun escape_byte/1, Data), ?FEND],
    lists:flatten(EncodedData).


decode(Ref, Byte) ->
    gen_fsm:send_event(Ref, {decode, Byte}).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {receiver, data}).


%% =============================================================================
%%  Callbacks for gen_fsm.
%% =============================================================================

init({Receiver}) ->
    {ok, idle, #state{receiver = Receiver}}.


%%
%%  State: idle
%%
idle({decode, ?FEND}, StateData) ->
    {next_state, frame_start, StateData#state{data = []}};

idle({decode, Byte}, StateData) when is_integer(Byte) ->
    {next_state, idle, StateData}.


%%
%%  State: frame_start
%%
frame_start({decode, ?FT_DATA}, StateData) ->
    {next_state, frame_data, StateData};

frame_start({decode, Byte}, StateData) when is_integer(Byte) ->
    {next_state, idle, StateData}.


%%
%%  State: frame_data
%%
frame_data({decode, ?FESC}, StateData) ->
    {next_state, frame_esc, StateData};

frame_data({decode, ?FEND}, StateData = #state{receiver = Receiver, data = Data}) ->
    ok = ls1mcs_protocol:received(Receiver, (lists:reverse(Data))),
    {next_state, idle, StateData};

frame_data({decode, Byte}, StateData = #state{data = Data}) when is_integer(Byte) ->
    NewStateData = StateData#state{data = [Byte | Data]},
    {next_state, frame_data, NewStateData}.


%%
%%  State: frame_esc
%%
frame_esc({decode, ?TFESC}, StateData = #state{data = Data}) ->
    NewStateData = StateData#state{data = [?FESC | Data]},
    {next_state, frame_data, NewStateData};

frame_esc({decode, ?TFEND}, StateData = #state{data = Data}) ->
    NewStateData = StateData#state{data = [?FEND | Data]},
    {next_state, frame_data, NewStateData};

frame_esc({decode, Byte}, StateData = #state{data = Data}) when is_integer(Byte) ->
    NewStateData = StateData#state{data = [Byte | Data]},
    {next_state, frame_data, NewStateData}.


%%
%%  Other FSM callbacks.
%%

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.


handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.


handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.


terminate(_Reason, _StateName, _StateData) ->
    ok.


code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.



%% =============================================================================
%%  Internal Functions.
%% =============================================================================

escape_byte(?FEND) -> [?FESC, ?TFEND];
escape_byte(?FESC) -> [?FESC, ?TFESC];
escape_byte(Byte) -> Byte.


%%
%%  Implementation of the KISS protocol.
%%  See http://www.ax25.net/kiss.aspx for more details.
%%
-module(ls1mcs_proto_kiss).
-behaviour(gen_fsm).
-behaviour(ls1mcs_protocol).
-export([start_link/3, send/2, received/2]). % Public API
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


%%
%%
%%
start_link(Name, Lower, Upper) ->
    gen_fsm:start_link({via, gproc, Name}, ?MODULE, {Lower, Upper}, []).


%%
%%
%%
send(Ref, Data) when is_binary(Data) ->
    gen_fsm:send_all_state_event({via, gproc, Ref}, {send, Data}).


%%
%%
%%
received(_Ref, <<>>) ->
    ok;

received(Ref, <<Byte:8, Rest/binary>>) ->
    ok = gen_fsm:send_event({via, gproc, Ref}, {received, Byte}),
    received(Ref, Rest).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {data, lower, upper}).


%% =============================================================================
%%  Callbacks for gen_fsm.
%% =============================================================================

init({Lower, Upper}) ->
    {ok, idle, #state{lower = Lower, upper = Upper}}.


%%
%%  State: idle
%%
idle({received, ?FEND}, StateData) ->
    {next_state, frame_start, StateData#state{data = []}};

idle({received, _Byte}, StateData) ->
    {next_state, idle, StateData}.


%%
%%  State: frame_start
%%
frame_start({received, ?FT_DATA}, StateData) ->
    {next_state, frame_data, StateData};

frame_start({received, _Byte}, StateData) ->
    {next_state, idle, StateData}.


%%
%%  State: frame_data
%%
frame_data({received, ?FESC}, StateData) ->
    {next_state, frame_esc, StateData};

frame_data({received, ?FEND}, StateData = #state{upper = Upper, data = Data}) ->
    ok = ls1mcs_protocol:received(Upper, list_to_binary(lists:reverse(Data))),
    {next_state, idle, StateData};

frame_data({received, Byte}, StateData = #state{data = Data}) ->
    NewStateData = StateData#state{data = [Byte | Data]},
    {next_state, frame_data, NewStateData}.


%%
%%  State: frame_esc
%%
frame_esc({received, ?TFESC}, StateData = #state{data = Data}) ->
    NewStateData = StateData#state{data = [?FESC | Data]},
    {next_state, frame_data, NewStateData};

frame_esc({received, ?TFEND}, StateData = #state{data = Data}) ->
    NewStateData = StateData#state{data = [?FEND | Data]},
    {next_state, frame_data, NewStateData};

frame_esc({received, Byte}, StateData = #state{data = Data}) ->
    NewStateData = StateData#state{data = [Byte | Data]},
    {next_state, frame_data, NewStateData}.


%%
%%  Handle encryption.
%%
handle_event({send, Data}, StateName, StateData = #state{lower = Lower}) ->
    ok = ls1mcs_protocol:send(Lower, encode(Data)),
    {next_state, StateName, StateData}.



%%
%%  Other FSM callbacks.
%%
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

encode(Data) ->
    InitialList = binary_to_list(Data),
    EncodedList = [?FEND, ?FT_DATA, lists:map(fun escape_byte/1, InitialList), ?FEND],
    list_to_binary(EncodedList).


escape_byte(?FEND) -> [?FESC, ?TFEND];
escape_byte(?FESC) -> [?FESC, ?TFESC];
escape_byte(Byte) -> Byte.


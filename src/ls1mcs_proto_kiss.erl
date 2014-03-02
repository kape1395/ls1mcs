%%
%%  Implementation of the KISS protocol.
%%  See http://www.ax25.net/kiss.aspx for more details.
%%
-module(ls1mcs_proto_kiss).
-behaviour(gen_fsm).
-behaviour(ls1mcs_protocol).
-compile([{parse_transform, lager_transform}]).
-export([start_link/3, send/2, received/2, preview/1]). % Public API
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



%%
%%
%%  Returns decoded frames.
%%  Used for TM preview.
%%
preview(Data) when is_binary(Data) ->
    preview(binary_to_list(Data));

preview(Data) when is_list(Data) ->
    {ok, Frames, _State} = decode(Data, undefined),
    {ok, Frames}.


%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {data, lower, upper}).


%% =============================================================================
%%  Callbacks for gen_fsm.
%% =============================================================================

init({Lower, Upper}) ->
    self() ! {initialize},
    {ok, idle, #state{data = [], lower = Lower, upper = Upper}}.


%%
%%  State: idle
%%
idle({received, ?FEND}, StateData = #state{data = Data}) ->
    lager:debug("Received FEND: clearing buffer=~p", [lists:reverse(Data)]),
    {next_state, frame_start, StateData#state{data = []}};

idle({received, Byte}, StateData) ->
    lager:debug("Received ~p", [Byte]),
    {next_state, idle, StateData}.


%%
%%  State: frame_start
%%
frame_start({received, ?FT_DATA}, StateData) ->
    lager:debug("Received FT_DATA"),
    {next_state, frame_data, StateData};

frame_start({received, ?FEND}, StateData = #state{data = Data}) ->
    lager:debug("Received FEND: clearing buffer=~p", [lists:reverse(Data)]),
    {next_state, frame_start, StateData#state{data = []}};

frame_start({received, Byte}, StateData) ->
    lager:debug("Received ~p, assuming its trash, going to idle.", [Byte]),
    {next_state, idle, StateData}.


%%
%%  State: frame_data
%%
frame_data({received, ?FESC}, StateData) ->
    {next_state, frame_esc, StateData};

frame_data({received, ?FEND}, StateData = #state{upper = Upper, data = Data}) ->
    AccumulatedData = list_to_binary(lists:reverse(Data)),
    lager:debug("Received FEND: sending accumulated data to the upper layer: ~p", [AccumulatedData]),
    ok = ls1mcs_protocol:received(Upper, AccumulatedData),
    {next_state, frame_start, StateData#state{data = []}};

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


handle_info({initialize}, StateName, StateData = #state{lower = Lower, upper = Upper}) ->
    ls1mcs_protocol:await(Upper),
    ls1mcs_protocol:await(Lower),
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



%%
%%  TODO: Use this function instead of FSM state transitions.
%%
decode(Data, undefined) ->
    decode(Data, {idle, []});

decode(Data, {InitState, InitPartial}) ->
    {State, Decoded, Partial} = lists:foldl(fun decode_fun/2, {InitState, [], InitPartial}, Data),
    Frames = lists:reverse([ list_to_binary(lists:reverse(D)) || D <- Decoded]),
    {ok, Frames, {State, Partial}}.

decode_fun(?FEND,    {idle,        Decoded, _Partial}) -> {frame_start, Decoded,              []};
decode_fun(_Byte,    {idle,        Decoded, _Partial}) -> {idle,        Decoded,              []};
decode_fun(?FT_DATA, {frame_start, Decoded, _Partial}) -> {frame_data,  Decoded,              []};
decode_fun(?FEND,    {frame_start, Decoded, _Partial}) -> {frame_start, Decoded,              []};
decode_fun(_Byte,    {frame_start, Decoded, _Partial}) -> {idle,        Decoded,              []};
decode_fun(?FESC,    {frame_data,  Decoded, Partial})  -> {frame_esc,   Decoded,              Partial};
decode_fun(?FEND,    {frame_data,  Decoded, Partial})  -> {frame_start, [ Partial | Decoded], []};
decode_fun(Byte,     {frame_data,  Decoded, Partial})  -> {frame_data,  Decoded,              [Byte  | Partial]};
decode_fun(?TFESC,   {frame_esc,   Decoded, Partial})  -> {frame_data,  Decoded,              [?FESC | Partial]};
decode_fun(?TFEND,   {frame_esc,   Decoded, Partial})  -> {frame_data,  Decoded,              [?FEND | Partial]};
decode_fun(Byte,     {frame_esc,   Decoded, Partial})  -> {frame_data,  Decoded,              [Byte  | Partial]}.



%%
%%  SAT Command FSM.
%%
%%  This process is registered in gproc with 2 keys:
%%      1. SatCmdId
%%      2. CRef = {Epoch, ProtocolCRef}
%%
-module(ls1mcs_sat_cmd).
-behaviour(gen_fsm).
-compile([{parse_transform, lager_transform}]).
-export([start_link/4, received/1]).
-export([sending/2, waiting_ack/2, receiving_data/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-include("ls1mcs.hrl").
-include("ls1p.hrl").

-define(REF(CmdFrameId), {via, gproc, {n, l, {?MODULE, CmdFrameId}}}).
-define(DATA_TIMEOUT, 600000).  % 600000 ms = 10 min



%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%  Frame should be already registered.
%%
start_link(SatCmd = #sat_cmd{id = SatCmdId}, UsrCmdRef, Ls1pRef, Sender) ->
    gen_fsm:start_link(?REF(SatCmdId), ?MODULE, {SatCmd, UsrCmdRef, Ls1pRef, Sender}, []).


%%
%%  Related SAT responses dispatched to this function.
%%
received(Ack = #ls1p_ack_frame{cref = CRef}) ->
    gen_fsm:send_event(?REF(CRef), {recv, Ack});

received(Data = #ls1p_data_frame{cref = CRef}) ->
    gen_fsm:send_event(?REF(CRef), {recv, Data}).




%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    sat_cmd     :: #sat_cmd{},      %% SAT command to send.
    usr_cmd_ref :: usr_cmd_ref(),   %% User command ID, that initiated this command.
    ls1p_ref    :: term()           %% Protocol ref to send frame to.
}).


%% =============================================================================
%%  Callbacks for gen_fsm.
%% =============================================================================

%%
%%
%%
init({SatCmd = #sat_cmd{id = SatCmdId}, UsrCmdRef, Ls1pRef, Sender}) ->
    erlang:link(Sender),
    gproc:reg({n, l, {?MODULE, cref(SatCmdId)}}),
    gen_fsm:send_event(self(), start),
    StateData = #state{
        sat_cmd = SatCmd,
        usr_cmd_ref = UsrCmdRef,
        ls1p_ref = Ls1pRef
    },
    {ok, sending, StateData}.


%%
%%  Asynchronously send command to the link.
%%  NOTE: We can implement awaiting of a session here.
%%
sending(start, StateData = #state{sat_cmd = SatCmd, ls1p_ref = Ls1pRef}) ->
    #sat_cmd{
        id = SatCmdId,
        cmd_frame = CmdFrame
    } = SatCmd,
    #ls1p_cmd_frame{
        ack = NeedsAck,
        delay = Delay
    } = CmdFrame,
    CmdFrameWithCRef = CmdFrame#ls1p_cmd_frame{cref = cref(SatCmdId)},
    lager:info("ls1mcs_sat_cmd: sending cmd frame: ~p", [CmdFrameWithCRef]),
    ok = ls1mcs_protocol:send(Ls1pRef, CmdFrameWithCRef),
    _TRef = gen_fsm:send_event_after(Delay * 1000 + ?DATA_TIMEOUT, timeout),
    case {NeedsAck, needs_data(CmdFrame)} of
        {true,  _}     -> {next_state, waiting_ack, StateData};
        {false, true}  -> {next_state, receiving_data, StateData};
        {false, false} -> {stop, normal, StateData}
    end.


%%
%%  Ack received. Either positive or negative.
%%
waiting_ack({recv, #ls1p_ack_frame{status = Status}}, StateData) ->
    #state{sat_cmd = SatCmd = #sat_cmd{id = SatCmdId}, usr_cmd_ref = UsrCmdRef} = StateData,
    case {Status, needs_data(SatCmd)} of
        {true, true} ->
            {next_state, receiving_data, StateData};
        {true, false} ->
            ok = ls1mcs_usr_cmd:sat_cmd_completed(UsrCmdRef, SatCmdId),
            {stop, normal, StateData};
        {false, true} ->
            ok = ls1mcs_usr_cmd:sat_cmd_failed(UsrCmdRef, SatCmdId),
            {stop, normal, StateData};
        {false, false} ->
            ok = ls1mcs_usr_cmd:sat_cmd_failed(UsrCmdRef, SatCmdId),
            {stop, normal, StateData}
    end;

waiting_ack(Event = {recv, #ls1p_data_frame{}}, StateData) ->
    receiving_data(Event, StateData);

waiting_ack(timeout, StateData) ->
    #state{sat_cmd = #sat_cmd{id = SatCmdId}, usr_cmd_ref = UsrCmdRef} = StateData,
    ok = ls1mcs_usr_cmd:sat_cmd_failed(UsrCmdRef, SatCmdId),
    {stop, normal, StateData}.


%%
%%
%%
receiving_data({recv, #ls1p_data_frame{eof = Eof}}, StateData) ->
    #state{sat_cmd = #sat_cmd{id = SatCmdId}, usr_cmd_ref = UsrCmdRef} = StateData,
    case Eof of
        true ->
            ok = ls1mcs_usr_cmd:sat_cmd_completed(UsrCmdRef, SatCmdId),
            {stop, normal, StateData};
        false ->
            {next_state, receiving_data}
    end;

receiving_data(timeout, StateData) ->
    #state{sat_cmd = #sat_cmd{id = SatCmdId}, usr_cmd_ref = UsrCmdRef} = StateData,
    ok = ls1mcs_usr_cmd:sat_cmd_completed(UsrCmdRef, SatCmdId),
    {stop, normal, StateData}.


%%
%%  Other FSM callbacks.
%%
handle_event(_Event, StateName, StateData = #state{}) ->
    {next_state, StateName, StateData}.


handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.



handle_info(_Event, StateName, StateData = #state{}) ->
    {next_state, StateName, StateData}.


terminate(_Reason, _StateName, _StateData) ->
    ok.


code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.



%% =============================================================================
%%  Internal Functions.
%% =============================================================================

%%
%%
%%
cref(Id) ->
    ls1mcs_store:cref_from_sat_cmd_id(Id).


%%
%%
%%
needs_data(#ls1p_cmd_frame{addr = arm,      port = downlink  }) -> true;
needs_data(#ls1p_cmd_frame{addr = arm,      port = runtime_tm}) -> true;
needs_data(#ls1p_cmd_frame{addr = arduino,  port = photo_meta}) -> true;
needs_data(#ls1p_cmd_frame{addr = arduino,  port = photo_data}) -> true;
needs_data(#ls1p_cmd_frame{}) -> false.



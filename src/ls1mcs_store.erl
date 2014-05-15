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
%%  Main persistent store for the LS1MCS.
%%
-module(ls1mcs_store).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0, is_installed/0, install/0, install/1, wait_for_tables/1]).
-export([
    cref_from_sat_cmd_id/1,
    get_ls1p_frame/1,
    add_ls1p_frame/3,
    add_unknown_frame/2,
    get_tm/1,
    load_predicted_passes/1,
    get_usr_cmds/1, get_usr_cmd/2, add_usr_cmd/1, set_usr_cmd_status/2,
    get_sat_cmds/1, add_sat_cmd/1,
    next_photo_cref/0
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include_lib("stdlib/include/qlc.hrl").
-include("ls1mcs.hrl").
-include("ls1p.hrl").

-define(ATTRS(R), {attributes, record_info(fields, R)}).


%% =============================================================================
%%  API Function Definitions
%% =============================================================================


%%
%%  Starts the store.
%%
-spec start_link() -> {ok, pid()} | term().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).


%%
%%  Checks, if the DB is installed.
%%
is_installed() ->
   try mnesia:table_info(ls1mcs_store_counter, all) of
        _ ->
            true
    catch
        _Type:_Error ->
            false
    end.


%%
%%  Creates the store schema.
%%
-spec install([atom()]) -> ok.
install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    create_tables(Nodes),
    rpc:multicall(Nodes, application, stop, [mnesia]),
    ok.


%%
%%  Creates the store schema on the current host only.
%%
-spec install() -> ok.
install() ->
    install([node()]).


%%
%%  Waits for all tables.
%%
-spec wait_for_tables(number()) -> ok | term().
wait_for_tables(Timeout) ->
    Tables = [
        ls1mcs_store_ls1p_sent,
        ls1mcs_store_ls1p_recv,
        ls1mcs_store_ls1p_tm,
        ls1mcs_store_ls1p_unkn,
        ls1mcs_store_pred_pass,
        ls1mcs_store_usr_cmd,
        ls1mcs_store_sat_cmd,
        ls1mcs_store_counter
    ],
    mnesia:wait_for_tables(Tables, Timeout).



%% =============================================================================
%%  Table definitions.
%% =============================================================================

%%
%%  LS1P commands sent to the SAT.
%%
-record(ls1mcs_store_ls1p_sent, {
    cref,
    frame,
    bytes,
    sent_time
}).

%%
%%  LS1P responses (ACKs and DATA frames) received from the SAT.
%%
-record(ls1mcs_store_ls1p_recv, {
    cref,
    frame,
    bytes,
    recv_time
}).

%%
%%  Received LS1P telemetry frames.
%%
-record(ls1mcs_store_ls1p_tm, {
    id,
    recv_time,
    frame,
    bytes,
    source
}).

%%
%%  Unrecognized received LS1P frames.
%%
-record(ls1mcs_store_ls1p_unkn, {
    recv_time,
    bytes
}).


%%
%%  Predicted passes.
%%
-record(ls1mcs_store_pred_pass, {
    orbit,
    pass
}).

%%
%%  User command.
%%
-record(ls1mcs_store_usr_cmd, {
    id,
    cmd
}).

%%
%%  SAT command.
%%
-record(ls1mcs_store_sat_cmd, {
    id,
    usr_cmd_id,
    cmd
}).

%%
%%  Misc counters.
%%
-record(ls1mcs_store_counter, {
    key,
    value
}).


%%
%%  Creates mnesia tables.
%%
create_tables(Nodes) ->
    DefOptDC = {disc_copies, Nodes},
    ORD = ordered_set,
    OK = {atomic, ok},
    OK = mnesia:create_table(ls1mcs_store_ls1p_sent, [{type, set}, ?ATTRS(ls1mcs_store_ls1p_sent), DefOptDC]),
    OK = mnesia:create_table(ls1mcs_store_ls1p_recv, [{type, bag}, ?ATTRS(ls1mcs_store_ls1p_recv), DefOptDC]),
    OK = mnesia:create_table(ls1mcs_store_ls1p_tm,   [{type, ORD}, ?ATTRS(ls1mcs_store_ls1p_tm),   DefOptDC]),
    OK = mnesia:create_table(ls1mcs_store_ls1p_unkn, [{type, bag}, ?ATTRS(ls1mcs_store_ls1p_unkn), DefOptDC]),
    OK = mnesia:create_table(ls1mcs_store_pred_pass, [{type, set}, ?ATTRS(ls1mcs_store_pred_pass), DefOptDC]),
    OK = mnesia:create_table(ls1mcs_store_usr_cmd,   [{type, set}, ?ATTRS(ls1mcs_store_usr_cmd),   DefOptDC]),
    OK = mnesia:create_table(ls1mcs_store_sat_cmd,   [{type, set}, ?ATTRS(ls1mcs_store_sat_cmd),   DefOptDC]),
    OK = mnesia:create_table(ls1mcs_store_counter,   [{type, set}, ?ATTRS(ls1mcs_store_counter),   DefOptDC]),
    OK = mnesia:add_table_index(ls1mcs_store_sat_cmd, usr_cmd_id),
    ok.



%% =============================================================================
%%  Query functions
%% =============================================================================


%%
%%  Get LS1P frame.
%%
get_ls1p_frame({cmd, all}) ->
    List = mnesia:dirty_match_object(#ls1mcs_store_ls1p_sent{_ = '_'}),
    {ok, [ Frame || #ls1mcs_store_ls1p_sent{frame = Frame} <- List ]};

get_ls1p_frame({cmd, {Epoch, CRef}}) ->
    case mnesia:dirty_read(ls1mcs_store_ls1p_sent, {Epoch, CRef}) of
        [] ->
            {error, not_found};
        [#ls1mcs_store_ls1p_sent{frame = Frame}] ->
            {ok, Frame}
    end;

get_ls1p_frame({ack, {Epoch, CRef}}) ->
    List = mnesia:dirty_read(ls1mcs_store_ls1p_recv, {Epoch, CRef}),
    {ok, [ Frame || #ls1mcs_store_ls1p_recv{frame = Frame} <- List, is_record(Frame, ls1p_ack_frame) ]};

get_ls1p_frame({data, {Epoch, CRef}}) ->
    List = mnesia:dirty_read(ls1mcs_store_ls1p_recv, {Epoch, CRef}),
    {ok, [ Frame || #ls1mcs_store_ls1p_recv{frame = Frame} <- List, is_record(Frame, ls1p_data_frame) ]};

get_ls1p_frame({recv, {Epoch, CRef}}) ->
    List = mnesia:dirty_read(ls1mcs_store_ls1p_recv, {Epoch, CRef}),
    {ok, [ Frame || #ls1mcs_store_ls1p_recv{frame = Frame} <- List ]}.


%%
%%  Add LS1P frame.
%%
add_ls1p_frame(Frame = #ls1p_cmd_frame{cref = {Epoch, CRef}}, Bytes, Timestamp) when is_integer(Epoch) ->
    Activity = fun () ->
        ok = mnesia:write(#ls1mcs_store_ls1p_sent{
            cref = {Epoch, CRef},
            frame = Frame,
            bytes = Bytes,
            sent_time = Timestamp
        }),
        {ok, Frame}
    end,
    mnesia:activity(transaction, Activity);

add_ls1p_frame(Frame = #ls1p_ack_frame{cref = {Epoch, CRef}}, Bytes, Timestamp) ->
    Activity = fun () ->
        ResolvedCRef = resolve_recv_cref(Epoch, CRef, Timestamp),
        ok = mnesia:write(#ls1mcs_store_ls1p_recv{
            cref = ResolvedCRef,
            frame = Frame#ls1p_ack_frame{cref = ResolvedCRef},
            bytes = Bytes,
            recv_time = Timestamp
        }),
        {ok, Frame#ls1p_ack_frame{cref = ResolvedCRef}}
    end,
    mnesia:activity(transaction, Activity);

add_ls1p_frame(Frame = #ls1p_data_frame{cref = {Epoch, CRef}}, Bytes, Timestamp) ->
    Activity = fun () ->
        ResolvedCRef = resolve_recv_cref(Epoch, CRef, Timestamp),
        ok = mnesia:write(#ls1mcs_store_ls1p_recv{
            cref = ResolvedCRef,
            frame = Frame#ls1p_data_frame{cref = ResolvedCRef},
            bytes = Bytes,
            recv_time = Timestamp
        }),
        {ok, Frame#ls1p_data_frame{cref = ResolvedCRef}}
    end,
    mnesia:activity(transaction, Activity);

add_ls1p_frame(Frame = #ls1p_tm_frame{recv = Recv}, Bytes, Timestamp) ->
    Id = mnesia:dirty_update_counter(ls1mcs_store_counter, tm, 1),
    NewRecv = case Recv of
        undefined -> Timestamp;
        {_, _, _} -> Recv
    end,
    Activity = fun () ->
        ok = mnesia:write(#ls1mcs_store_ls1p_tm{
            id = Id,
            frame = Frame#ls1p_tm_frame{id = Id, recv = NewRecv},
            bytes = Bytes,
            recv_time = Timestamp,
            source = gs
        }),
        {ok, Frame}
    end,
    mnesia:activity(transaction, Activity).


%%
%%  Add unknown frame (not recognized as a valid LS1P frame).
%%
add_unknown_frame(Bytes, Timestamp) ->
    Activity = fun () ->
        ok = mnesia:write(#ls1mcs_store_ls1p_unkn{
            bytes = Bytes,
            recv_time = Timestamp
        }),
        ok
    end,
    mnesia:activity(transaction, Activity).


%%
%%  Get TM.
%%
get_tm(all) ->
    Records = mnesia:dirty_match_object(#ls1mcs_store_ls1p_tm{source = gs, _ = '_'}),
    Frames = [ Frame || #ls1mcs_store_ls1p_tm{frame = Frame} <- Records ],
    {ok, Frames};

get_tm(latest) ->
    Activity = fun () ->
        case mnesia:last(ls1mcs_store_ls1p_tm) of
            '$end_of_table' ->
                {ok, []};
            LastKey ->
                [#ls1mcs_store_ls1p_tm{frame = Frame}] = mnesia:read(ls1mcs_store_ls1p_tm, LastKey),
                {ok, [Frame]}
        end
    end,
    mnesia:activity(transaction, Activity);

get_tm({time, From, Till}) ->
    MatchHead = #ls1mcs_store_ls1p_tm{source = gs, frame = '$1', recv_time = '$2', _ = '_'},
    Guards = case {From, Till} of
        {undefined, undefined} -> [];
        {From,      undefined} -> [{'>=', '$2', From}];
        {undefined, Till     } -> [{'<',  '$2', Till}];
        {From,      Till     } -> [{'>=', '$2', From}, {'<', '$2', Till}]
    end,
    Frames = mnesia:dirty_select(ls1mcs_store_ls1p_tm, [{MatchHead, Guards, ['$1']}]),
    {ok, Frames}.


%%
%%  Load (add new, overwrite existing) predicred passes.
%%
load_predicted_passes(PredictedPasses) ->
    Activity = fun () ->
        SaveEntryFun = fun (Pass = #predicted_pass{orbit = Orbit}) ->
            ok = mnesia:write(#ls1mcs_store_pred_pass{
                orbit = Orbit,
                pass = Pass
            })
        end,
        lists:foreach(SaveEntryFun, PredictedPasses),
        ok
    end,
    mnesia:activity(transaction, Activity).


%%
%%  Get user commands.
%%
get_usr_cmds(all) ->
    Records = mnesia:dirty_match_object(#ls1mcs_store_usr_cmd{_ = '_'}),
    Cmds = [ C || #ls1mcs_store_usr_cmd{cmd = C} <- Records ],
    {ok, Cmds};

get_usr_cmds({id, Id}) ->   % TODO: Remove (use get_usr_cmd).
    case mnesia:dirty_read(ls1mcs_store_usr_cmd, Id) of
        [] ->
            {ok, []};
        [#ls1mcs_store_usr_cmd{cmd = C}] ->
            {ok, [C]}
    end.


%%
%%
%%
-spec get_usr_cmd(
            Id :: integer(),
            Query :: term()
        ) ->
        {ok, #usr_cmd{}, [{#sat_cmd{}, #ls1p_cmd_frame{}, #ls1p_ack_frame{}, [#ls1p_data_frame{}]}]} |
        {error, not_found}.

get_usr_cmd(UsrCmdId, all) ->
    case mnesia:dirty_read(ls1mcs_store_usr_cmd, UsrCmdId) of
        [] ->
            {error, not_found};
        [#ls1mcs_store_usr_cmd{cmd = UsrCmd}] ->
            SatCmdRecs = mnesia:dirty_index_read(ls1mcs_store_sat_cmd, UsrCmdId, #ls1mcs_store_sat_cmd.usr_cmd_id),
            LoadSatCmdFramesFun = fun(#ls1mcs_store_sat_cmd{id = SatCmdId, cmd = SatCmd}) ->
                CRef = cref_from_sat_cmd_id(SatCmdId),
                case get_ls1p_frame({cmd, CRef}) of
                    {ok, CmdFrame} -> CmdFrame;
                    {error, not_found} -> CmdFrame = undefined
                end,
                case get_ls1p_frame({ack, CRef}) of
                    {ok, [AckFrame]} -> AckFrame;
                    {ok, []} -> AckFrame = undefined
                end,
                {ok, DataFrames} = get_ls1p_frame({data, CRef}),
                {SatCmd, CmdFrame, AckFrame, DataFrames}
            end,
            SatCmds = lists:map(LoadSatCmdFramesFun, SatCmdRecs),
            {ok, UsrCmd, SatCmds}
    end.


%%
%%  Add new or update existing user command.
%%
add_usr_cmd(UserCmd = #usr_cmd{id = SuppliedId}) ->
    Id = case SuppliedId of
        undefined -> mnesia:dirty_update_counter(ls1mcs_store_counter, usr_cmd, 1);
        _         -> SuppliedId
    end,
    Activity = fun () ->
        ok = mnesia:write(#ls1mcs_store_usr_cmd{
            id = Id,
            cmd = UserCmd#usr_cmd{id = Id}
        }),
        {ok, Id}
    end,
    mnesia:activity(transaction, Activity).


%%
%%  Update user command status.
%%
set_usr_cmd_status(UsrCmdId, Status) ->
    Activity = fun () ->
        [Rec = #ls1mcs_store_usr_cmd{cmd = UsrCmd}] = mnesia:read(ls1mcs_store_usr_cmd, UsrCmdId),
        ok = mnesia:write(Rec#ls1mcs_store_usr_cmd{cmd = UsrCmd#usr_cmd{status = Status}})
    end,
    mnesia:activity(transaction, Activity).


%%
%%  Get SAT commands.
%%
get_sat_cmds(all) ->
    Records = mnesia:dirty_match_object(#ls1mcs_store_sat_cmd{_ = '_'}),
    Cmds = [ C || #ls1mcs_store_sat_cmd{cmd = C} <- Records ],
    {ok, Cmds};

get_sat_cmds({id, Id}) ->
    case mnesia:dirty_read(ls1mcs_store_sat_cmd, Id) of
        [] ->
            {ok, []};
        [#ls1mcs_store_sat_cmd{cmd = C}] ->
            {ok, [C]}
    end;

get_sat_cmds({usr_cmd, UsrCmdId}) ->
    Records = mnesia:dirty_match_object(#ls1mcs_store_sat_cmd{usr_cmd_id = UsrCmdId, _ = '_'}),
    Cmds = [ C || #ls1mcs_store_sat_cmd{cmd = C} <- Records ],
    {ok, Cmds}.


%%
%%  Add new or update existing SAT command.
%%
add_sat_cmd(SatCmd = #sat_cmd{id = SuppliedId, usr_cmd_id = UsrCmdId}) ->
    Id = case SuppliedId of
        undefined -> mnesia:dirty_update_counter(ls1mcs_store_counter, sat_cmd, 1);
        _         -> SuppliedId
    end,
    Activity = fun () ->
        ok = mnesia:write(#ls1mcs_store_sat_cmd{
            id = Id,
            usr_cmd_id = UsrCmdId,
            cmd = SatCmd#sat_cmd{id = Id}
        }),
        {ok, Id}
    end,
    mnesia:activity(transaction, Activity).


%%
%%
%%
next_photo_cref() ->
    mnesia:dirty_update_counter(ls1mcs_store_counter, photo_cref, 1).



%% =============================================================================
%%  Internal state of this module.
%% =============================================================================


-record(state, {
}).



%% =============================================================================
%%  Callbacks for gen_server (unused).
%% =============================================================================


init({}) ->
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {reply, undefined, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% =============================================================================
%%  Helper functions.
%% =============================================================================

%%
%%
%%
cref_from_sat_cmd_id(Id) ->
    CRef = Id rem 16#FFFF,
    Epoch = Id div 16#FFFF,
    {Epoch, CRef}.


%%
%%  Resolve unknown epoch. Avoid duplicate keys.
%%
resolve_recv_cref(Epoch, CRef, _Timestamp) when is_integer(Epoch) ->
    {Epoch, CRef};

resolve_recv_cref(undefined, CRef, Timestamp) ->
    case mnesia:dirty_read(ls1mcs_store_counter, sat_cmd) of
        [#ls1mcs_store_counter{value = This}] ->
            {ThisEpoch, ThisCRef} = cref_from_sat_cmd_id(This),
            case ThisCRef >= CRef of
                true -> resolve_recv_cref(ThisEpoch, CRef, Timestamp);
                false -> resolve_recv_cref(ThisEpoch - 1, CRef, Timestamp)
            end;
        [] ->
            resolve_recv_cref(-1, CRef, Timestamp)
    end.




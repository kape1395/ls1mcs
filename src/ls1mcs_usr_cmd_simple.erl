%%
%%  User command: simple parametrizable command.
%%  Executes single SAT command.
%%
-module(ls1mcs_usr_cmd_simple).
-behaviour(ls1mcs_usr_cmd).
-behaviour(gen_fsm).
-compile([{parse_transform, lager_transform}]).
-export([start_link/2]).
-export([sat_cmd_status/3]).
-export([starting/2, executing/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-include("ls1mcs.hrl").

-define(REF(UsrCmdId), {via, gproc, {n, l, {?MODULE, UsrCmdId}}}).


%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%
%%
-spec start_link(#usr_cmd{}, #usr_cmd_spec{})
        -> {ok, pid()} | term().

start_link(UsrCmd = #usr_cmd{id = UsrCmdId}, UsrCmdSpec) ->
    SatCmd = mk_sat_cmd(UsrCmd, UsrCmdSpec),
    gen_fsm:start_link(?REF(UsrCmdId), ?MODULE, {UsrCmd, SatCmd}, []).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    usr_cmd     :: #usr_cmd{},   %% User command id.
    sat_cmd     :: #sat_cmd{}    %% Sat command issued by this user command.
}).



%% =============================================================================
%%  Callbacks for ls1mcs_usr_cmd.
%% =============================================================================

%%
%%
%%
sat_cmd_status(UsrCmdId, SatCmdId, Status) ->
    ok = gen_fsm:send_event(?REF(UsrCmdId), {sat_cmd_status, SatCmdId, Status}).



%% =============================================================================
%%  Callbacks for gen_fsm.
%% =============================================================================

%%
%%
%%
init({UsrCmd, SatCmd}) ->
    gen_fsm:send_event(self(), start),
    StateData = #state{
        usr_cmd = UsrCmd,
        sat_cmd = SatCmd
    },
    {ok, starting, StateData}.


%%
%%  FSM State: starting.
%%
starting(start, StateData = #state{usr_cmd = #usr_cmd{id = UsrCmdId}, sat_cmd = SatCmd}) ->
    {ok, SatCmdId} = ls1mcs_usr_cmd:send_sat_cmd(?MODULE, UsrCmdId, SatCmd),
    NewStateData = StateData#state{
        sat_cmd = SatCmd#sat_cmd{id = SatCmdId}
    },
    {next_state, executing, NewStateData}.


%%
%%  FSM State: executing.
%%
executing({sat_cmd_status, _SatCmdId, failed}, StateData = #state{usr_cmd = UsrCmd, sat_cmd = SatCmd}) ->
    lager:debug("User command failed, usr_cmd=~p, sat_cmd=~p", [UsrCmd, SatCmd]),
    {stop, normal, StateData};

executing({sat_cmd_status, _SatCmdId, completed}, StateData = #state{usr_cmd = UsrCmd, sat_cmd = SatCmd}) ->
    lager:debug("User command completed, usr_cmd=~p, sat_cmd=~p", [UsrCmd, SatCmd]),
    {stop, normal, StateData}.


%%
%%  Other FSM callbacks.
%%
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.



%% =============================================================================
%%  Internal Functions.
%% =============================================================================

%%
%%  Creates LS1P command frame for the specified user command.
%%
mk_sat_cmd(#usr_cmd{args = Args}, #usr_cmd_spec{name = SpecName}) ->
    case SpecName of
        ping ->
            #sat_cmd{
                cmd_frame = #ls1p_cmd_frame{
                    addr = arm,
                    port = ping,
                    ack = true
                }
            };
        kill ->
            CRef = arg_val(cref, Args),
            #sat_cmd{
                cmd_frame = #ls1p_cmd_frame{
                    addr = arm,
                    port = kill,
                    ack = true,
                    data = <<CRef:16/little>>
                }
            };
        downlink ->
            BufId = arg_val(bufid, Args),
            BlkSz = arg_val(blksz, Args),
            From = arg_val(from, Args),
            Till = arg_val(till, Args),
            #sat_cmd{
                cmd_frame = #ls1p_cmd_frame{
                    addr = arm,
                    port = downlink,
                    ack = false,
                    data = <<BufId:8, BlkSz:8, From:16/little, Till:16/little>>
                },
                exp_dfc = Till - From
            };
        runtime_tm ->
            #sat_cmd{
                cmd_frame = #ls1p_cmd_frame{
                    addr = arm,
                    port = runtime_tm,
                    ack = false
                },
                exp_dfc = 1
            };
        job_period ->
            JobId = arg_val(jobid, Args),
            Interval  = arg_val(interval, Args),
            #sat_cmd{
                cmd_frame = #ls1p_cmd_frame{
                    addr = arm,
                    port = job_period,
                    ack = false,
                    data = <<JobId:8, Interval:16/little>>
                }
            };
        take_photo ->
            ResId = arg_val(resid, Args),
            Delay = arg_val(delay, Args),
            PhotoCRef = ls1mcs_store:next_photo_cref(),
            #sat_cmd{
                cmd_frame = #ls1p_cmd_frame{
                    addr = arduino,
                    port = take_photo,
                    ack = true,
                    delay = Delay,
                    data = <<PhotoCRef:16/little, ResId:8>>
                }
            };
        photo_meta ->
            #sat_cmd{
                cmd_frame = #ls1p_cmd_frame{
                    addr = arduino,
                    port = photo_meta,
                    ack = false
                },
                exp_dfc = 1
            };
        photo_data ->
            BlkSz = arg_val(blksz, Args),
            From = arg_val(from, Args),
            Till = arg_val(till, Args),
            #sat_cmd{
                cmd_frame = #ls1p_cmd_frame{
                    addr = arduino,
                    port = photo_data,
                    ack = false,
                    data = <<BlkSz:8, From:16/little, Till:16/little>>
                },
                exp_dfc = Till - From
            }
    end.


%%
%%  Returns user command argument as an integer.
%%
arg_val(Name, Args) ->
    #usr_cmd_arg{name = Name, value = Value} = lists:keyfind(Name, #usr_cmd_arg.name, Args),
    erlang:binary_to_integer(Value).


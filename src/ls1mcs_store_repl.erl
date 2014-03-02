-module(ls1mcs_store_repl).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).

-export([
    start_link/0,
    send_telemetry/0
]).

%% gen_server callbacks
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2, code_change/3]).

-include("ls1mcs.hrl").
-include("ls1p.hrl").

%% =============================================================================
%%  Internal state
%% =============================================================================

-record(state, {
    ibrowse_pid
}).

%% =============================================================================
%%  Starts the store.
%% =============================================================================
-spec start_link() -> {ok, pid()} | term().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).


%% =============================================================================
%%  Public functions
%% =============================================================================

send_telemetry() ->
    gen_server:cast( self(), { send_telemetry } ).


%% =============================================================================
%%  Callbacks for gen_server
%% =============================================================================

init({}) ->
    { ok, Pid } = ibrowse:start(),

    %% Test sending telemetry
    gen_server:cast( self(), { send_telemetry } ),

    { ok, #state{
        ibrowse_pid = Pid
    } }.

handle_call(_Msg, _From, State) ->
    { reply, undefined, State }.

handle_cast( { send_telemetry }, State ) ->
    case ls1mcs_store:get_tm(latest) of
        { ok, [TmFrame] } ->
            Tm = ls1mcs_yaws_json:encode( TmFrame ),
            %% TODO: Move hardcoded values to config
            ibrowse:send_req(
                "http://localhost:8234/api/telemetry", [], put,
                jiffy:encode( Tm )
            ),
            lager:warning( "Tm: ~p", [ Tm ] );
        { ok, [] } -> ok
    end,
    { noreply, State };

handle_cast(_Msg, State) ->
    { noreply, State }.

handle_info(_Msg, State) ->
    { noreply, State }.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

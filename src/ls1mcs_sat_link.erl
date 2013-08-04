%%
%%  Entry point to SAT connection / link.
%%
-module(ls1mcs_sat_link).
-behaviour(gen_server).
-behaviour(ls1mcs_protocol).
-compile([{parse_transform, lager_transform}]).
-export([start_link/2, send/2]).
-export([received/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("ls1p.hrl").
-include("ls1mcs.hrl").


%% =============================================================================
%%  API Function Definitions
%% =============================================================================

%%
%%  Start the connection.
%%
start_link(Name, Ls1pRef) ->
    {ok, Pid} = gen_server:start_link({via, gproc, Name}, ?MODULE, {Ls1pRef}, []),
    true = erlang:register(?MODULE, Pid),
    {ok, Pid}.



%%
%%  Send command to the SAT.
%%  Links the caller with the SAT CMD sending process.
%%
-spec send(#sat_cmd{}, usr_cmd_ref())
        -> {ok, sat_cmd_id()}.

send(SatCmd, UsrCmdRef) when is_record(SatCmd, sat_cmd) ->
    gen_server:call(?MODULE, {send, SatCmd, UsrCmdRef, self()}).


%% =============================================================================
%%  Internal data structures.
%% =============================================================================


-record(state, {
    ls1p
}).



%% =============================================================================
%%  Callbacks for ls1mcs_protocol.
%% =============================================================================

%%
%%  ls1mcs_protocol:send/2 is not used here.
%%


%%
%%  Receives incoming messages from the protocol stack.
%%
received(Ref, Ls1pFrame) ->
    gen_server:call({via, gproc, Ref}, {received, Ls1pFrame}).



%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================

%%
%%
%%
init({Ls1pRef}) ->
    {ok, #state{ls1p = Ls1pRef}}.


%%
%%
%%
handle_call({send, SatCmd, UsrCmdRef, Sender}, _From, State = #state{ls1p = Ls1pRef}) ->
    {ok, SatCmdId} = ls1mcs_store:add_sat_cmd(SatCmd),
    SatCmdWithId = SatCmd#sat_cmd{id = SatCmdId},
    lager:info("ls1mcs_sat_link: sending sat cmd: ~p", [SatCmdWithId]),
    {ok, _Pid} = ls1mcs_sat_cmd_sup:add(SatCmdWithId, UsrCmdRef, Ls1pRef, Sender),
    {reply, {ok, SatCmdId}, State};

handle_call({received, Ack = #ls1p_ack_frame{}}, _From, State) ->
    lager:info("ls1mcs_sat_link: received sat ack: ~p", [Ack]),
    ok = ls1mcs_sat_cmd:received(Ack),
    {reply, ok, State};

handle_call({received, Data = #ls1p_data_frame{}}, _From, State) ->
    lager:info("ls1mcs_sat_link: received sat data: ~p", [Data]),
    ok = ls1mcs_sat_cmd:received(Data),
    {reply, ok, State};

handle_call({received, TM = #ls1p_tm_frame{}}, _From, State) ->
    lager:info("ls1mcs_sat_link: received sat TM: ~p", [TM]),
    {reply, ok, State}.


%%
%%
%%
handle_cast(_Msg, State) ->
    {noreply, State}.


%%
%%
%%
handle_info(_Info, State) ->
    {noreply, State}.


%%
%%
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


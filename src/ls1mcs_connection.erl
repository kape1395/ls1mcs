%%
%%  Entry point to SAT connection / link.
%%
-module(ls1mcs_connection).
-behaviour(gen_server).
-behaviour(ls1mcs_protocol).
-compile([{parse_transform, lager_transform}]).
-export([start_link/2, send/1]).
-export([send/2, received/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("ls1p.hrl").
-include("ls1mcs.hrl").


%% =============================================================================
%%  API Function Definitions
%% =============================================================================

%%
%%  Start the connection.
%%
start_link(Name, LinkRef) ->
    gen_server:start_link({via, gproc, Name}, ?MODULE, {LinkRef}, []).


%%
%%  Send command to the SAT.
%%
send(Ls1pCmdFrame) when is_record(Ls1pCmdFrame, ls1p_cmd_frame) ->
    gen_server:call(?MODULE, {send, Ls1pCmdFrame}).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================


-record(state, {
    link
}).



%% =============================================================================
%%  Callbacks for ls1mcs_protocol.
%% =============================================================================

%%
%%  Not used here.
%%
send(some_invalid_reference, some_invalid_data) ->
    ok.


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
init({LinkRef}) ->
    true = erlang:register(?MODULE, self()),
    {ok, #state{link = LinkRef}}.


%%
%%
%%
handle_call({send, Ls1pCmdFrame}, _From, State = #state{link = LinkRef}) ->
    {ok, {Epoch, CRef}} = ls1mcs_store:next_cref(),
    FrameToSend = Ls1pCmdFrame#ls1p_cmd_frame{cref = {Epoch, CRef}},
    lager:debug("ls1mcs_connection: sending command frame: ~p", [FrameToSend]),
    ok = ls1mcs_protocol:send(LinkRef, FrameToSend),
    {reply, {ok, {Epoch, CRef}}, State};

handle_call({received, Frame}, _From, State) when is_record(Frame, ls1p_cmd_frame) ->
    lager:debug("ls1mcs_connection: dropping received (echoed?) command frame: ~p", [Frame]),
    {reply, ok, State};

handle_call({received, Frame}, _From, State) ->
    lager:info("ls1mcs_connection: received a frame: ~p", [Frame]),
    % TODO
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


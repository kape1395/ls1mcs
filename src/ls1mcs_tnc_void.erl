%%
%%  Void TNC, does nothing apart from logging.
%%
-module(ls1mcs_tnc_void).
-behaviour(ls1mcs_tnc).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/1, send/2, received/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("ls1mcs.hrl").


%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%
%%
start_link(Name) ->
    gen_server:start_link({via, gproc, Name}, ?MODULE, {}, []).


%%
%%
%%
send(_Ref, Data) ->
    lager:info("ls1mcs_tnc_void: got request to send frame: ~p", [Data]),
    ok.


%%
%%  Not used here.
%%
received(_Ref, _Data) ->
    ok.



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {}).


%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================


%%
%%
%%
init({}) ->
    {ok, #state{}}.


%%
%%
%%
handle_call(_Message, _From, State) ->
    {stop, not_implemented, State}.


%%
%%
%%
handle_cast(_Message, State) ->
    {stop, not_implemented, State}.


%%
%%
%%
handle_info(_Message, State) ->
    {stop, not_implemented, State}.


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


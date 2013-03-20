-module(ls1mcs_proto_ls1p).
-behaviour(gen_server).
-behaviour(ls1mcs_protocol).
-export([start_link/3, send/2, received/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("ls1mcs.hrl").

-define(RECV_COUNT, 1).
-define(RECV_TIMEOUT, 1000).


%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%
%%
start_link(Name, Lower, Upper) ->
    gen_server:start_link({via, gproc, Name}, ?MODULE, {Lower, Upper}, []).


%%
%%
%%
send(Ref, Data) when is_binary(Data) ->
    gen_server:cast({via, gproc, Ref}, {send, Data}).


%%
%%  Not used here.
%%
received(Ref, Data) when is_binary(Data) ->
    gen_server:cast({via, gproc, Ref}, {send, Data}).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    lower,      %% Lower protocol ref.
    upper       %% Upper protocol ref.
}).


%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================


%%
%%
%%
init({Lower, Upper}) ->
    self() ! {initialize},
    {ok, #state{lower = Lower, upper = Upper}}.


%%
%%
%%
handle_call(_Message, _From, State) ->
    {stop, not_implemented, State}.


%%
%%  TODO:
%%
handle_cast({send, _Data}, State = #state{}) ->
    {noreply, State};

handle_cast({received, _Data}, State = #state{}) ->
    {noreply, State}.


%%
%%  Deffered initialization.
%%
handle_info({initialize}, State = #state{lower = Lower, upper = Upper}) ->
    ls1mcs_protocol:await(Lower),
    ls1mcs_protocol:await(Upper),
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

encode() ->
    ok.

decode() ->
    ok.


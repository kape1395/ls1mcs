-module(ls1mcs_connection).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% =============================================================================
%%  API Function Definitions
%% =============================================================================

%%
%%
%%
start_link(Name) ->
    gen_server:start_link(Name, ?MODULE, {}, []).



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
    {stop, error, State}.


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


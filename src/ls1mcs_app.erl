-module(ls1mcs_app).
-behaviour(application).
-export([start/2, stop/1]).


%% =============================================================================
%%  Application callbacks
%% =============================================================================


%%  @doc
%%  Start the application.
%%
start(_StartType, _StartArgs) ->
    ls1mcs_sup:start_link([]).


%%  @doc
%%  Stop the application.
%%
stop(_State) ->
    ok.



%% =============================================================================
%%  Helper functions.
%% =============================================================================


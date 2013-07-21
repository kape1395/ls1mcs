-module(ls1mcs_app).
-behaviour(application).
-export([start/2, stop/1]).

-define(APP, ls1mcs).

%% =============================================================================
%%  Application callbacks
%% =============================================================================


%%  @doc
%%  Start the application.
%%
start(_StartType, _StartArgs) ->
    {ok, LinkCfg = {_Type, _Options}} = application:get_env(?APP, link),
    GPredictCfg = application:get_env(?APP, load_gpredict_passes, undefined),
    ls1mcs_sup:start_link(LinkCfg, GPredictCfg).


%%  @doc
%%  Stop the application.
%%
stop(_State) ->
    ok.



%% =============================================================================
%%  Helper functions.
%% =============================================================================


-module(ls1mcs_sup).
-behaviour(supervisor).
-export([start_link/2]).
-export([init/1]).


%% =============================================================================
%%  API functions.
%% =============================================================================


%%
%% @doc Create this supervisor.
%%
start_link(LinkCfg, GPredictCfg) ->
    supervisor:start_link(?MODULE, {LinkCfg, GPredictCfg}).



%% =============================================================================
%%  Callbacks for supervisor.
%% =============================================================================


%%
%% @doc Supervisor initialization (CB).
%%
init({LinkCfg, GPredictCfg}) ->
    {LinkType, LinkOptions} = LinkCfg,

    ConnMod = ls1mcs_connection,
    LSupMod = ls1mcs_link_sup,
    StoreMod = ls1mcs_store,

    LinkRef = LSupMod:top_ref(),
    ConnName = {n, l, ConnMod},
    ConnRef  = ls1mcs_protocol:make_ref(ConnMod, ConnName),

    ConnArgs = [ConnName, LinkRef],
    LSupArgs = [ConnRef, LinkType, LinkOptions],

    ChildSpecs = [
        {store, {StoreMod, start_link, []},       permanent, 5000, worker,     [StoreMod]},
        {link,  {LSupMod,  start_link, LSupArgs}, permanent, 5000, supervisor, [LSupMod]},
        {conn,  {ConnMod,  start_link, ConnArgs}, permanent, 5000, worker,     [ConnMod]}
    ],

    GPredictSpec = case GPredictCfg of
        undefined ->
            [];
        {Filename, Interval} ->
            [{gpredict,
                {ls1mcs_gpredict,  start_link, [Filename, Interval]},
                permanent, 5000, worker, [ls1mcs_gpredict]
            }]
    end,

    {ok, {{one_for_all, 100, 10}, ChildSpecs ++ GPredictSpec}}.



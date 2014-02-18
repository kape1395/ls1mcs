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

    SLnkMod = ls1mcs_sat_link,
    LSupMod = ls1mcs_link_sup,  % Proto sup.
    SCmdMod = ls1mcs_sat_cmd_sup,
    UCmdMod = ls1mcs_usr_cmd_sup,
    StoreMod = ls1mcs_store,
    ReplMod = ls1mcs_store_repl,

    LinkRef = LSupMod:top_ref(),
    SLnkName = {n, l, SLnkMod},
    SLnkRef  = ls1mcs_protocol:make_ref(SLnkMod, SLnkName),

    SLnkArgs = [SLnkName, LinkRef],
    LSupArgs = [SLnkRef, LinkType, LinkOptions],

    ChildSpecs = [
        {store, {StoreMod, start_link, []},       permanent, 5000, worker,     [StoreMod]},
        {link,  {LSupMod,  start_link, LSupArgs}, permanent, 5000, supervisor, [LSupMod]},
        {scmd,  {SCmdMod,  start_link, []},       permanent, 5000, supervisor, [SCmdMod]},
        {ucmd,  {UCmdMod,  start_link, []},       permanent, 5000, supervisor, [UCmdMod]},
        {slnk,  {SLnkMod,  start_link, SLnkArgs}, permanent, 5000, worker,     [SLnkMod]},
        {repl,  {ReplMod,  start_link, []},       permanent, 5000, worker,     [ReplMod]}
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



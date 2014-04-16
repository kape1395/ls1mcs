%/--------------------------------------------------------------------
%| Copyright 2013-2014 Karolis Petrauskas
%|
%| Licensed under the Apache License, Version 2.0 (the "License");
%| you may not use this file except in compliance with the License.
%| You may obtain a copy of the License at
%|
%|     http://www.apache.org/licenses/LICENSE-2.0
%|
%| Unless required by applicable law or agreed to in writing, software
%| distributed under the License is distributed on an "AS IS" BASIS,
%| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%| See the License for the specific language governing permissions and
%| limitations under the License.
%\--------------------------------------------------------------------

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
    LinkMod = ls1mcs_sat_link_sup,
    SCmdMod = ls1mcs_sat_cmd_sup,
    UCmdMod = ls1mcs_usr_cmd_sup,
    StoreMod = ls1mcs_store,

    LinkArgs = [LinkCfg],

    ChildSpecs = [
        {store, {StoreMod, start_link, []},       permanent, 5000, worker,     [StoreMod]},
        {link,  {LinkMod,  start_link, LinkArgs}, permanent, 5000, supervisor, [LinkMod]},
        {scmd,  {SCmdMod,  start_link, []},       permanent, 5000, supervisor, [SCmdMod]},
        {ucmd,  {UCmdMod,  start_link, []},       permanent, 5000, supervisor, [UCmdMod]}
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



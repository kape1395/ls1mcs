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

%%
%%  Supervises all currently executing SAT commands.
%%
-module(ls1mcs_sat_cmd_sup).
-behaviour(supervisor).
-export([start_link/0, add/3]).
-export([init/1]).


%% =============================================================================
%%  API functions.
%% =============================================================================


%%
%%  Create this supervisor.
%%
start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, {}),
    ok = ls1mcs_sat_cmd_sll:register(),
    {ok, Pid}.


%%
%%  Start new command execution.
%%
add(SatCmd, UsrCmdRef, IssuedByPid) ->
    supervisor:start_child(?MODULE, [SatCmd, UsrCmdRef, IssuedByPid]).



%% =============================================================================
%%  Callbacks for supervisor.
%% =============================================================================


%%
%%  Supervisor initialization.
%%
init({}) ->
    Mod = ls1mcs_sat_cmd,
    {ok, {{simple_one_for_one, 100, 10}, [
        {Mod, {Mod, start_link, []}, temporary, 5000, worker, [Mod]}
    ]}}.



%%
%%  Supervises all currently executing SAT commands.
%%
-module(ls1mcs_sat_cmd_sup).
-behaviour(supervisor).
-export([start_link/0, add/4]).
-export([init/1]).


%% =============================================================================
%%  API functions.
%% =============================================================================


%%
%%  Create this supervisor.
%%
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).


%%
%%  Start new command execution.
%%
add(SatCmd, UsrCmdRef, Ls1pRef, Sender) ->
    supervisor:start_child(?MODULE, [SatCmd, UsrCmdRef, Ls1pRef, Sender]).



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



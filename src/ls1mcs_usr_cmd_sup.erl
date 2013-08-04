%%
%%  Supervises all currently executing User commands.
%%
-module(ls1mcs_usr_cmd_sup).
-behaviour(supervisor).
-export([start_link/0, add/1]).
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
add(MFA) ->
    supervisor:start_child(?MODULE, [MFA]).



%% =============================================================================
%%  Callbacks for supervisor.
%% =============================================================================


%%
%%  Supervisor initialization.
%%
init({}) ->
    Mod = ls1mcs_usr_cmd,
    {ok, {{simple_one_for_one, 100, 10}, [
        {Mod, {Mod, start_link, []}, temporary, 5000, worker, [Mod]}
    ]}}.



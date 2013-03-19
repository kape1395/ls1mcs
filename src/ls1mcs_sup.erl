-module(ls1mcs_sup).
-behaviour(supervisor).
-export([start_link/1]).    % API
-export([init/1]).          % CB


%% =============================================================================
%%  API functions.
%% =============================================================================


%%
%% @doc Create this supervisor.
%%
-spec start_link(Config :: term()) -> {ok, pid()} | term().
start_link(Config) ->
    supervisor:start_link(?MODULE, Config).



%% =============================================================================
%%  Callbacks for supervisor.
%% =============================================================================


%%
%% @doc Supervisor initialization (CB).
%%
-spec init(Config :: term()) -> tuple().
init(_Config) ->
    LinkName = {via, gproc, {n, l, ls1mcs_ls1p}},
    {ok, {{one_for_all, 100, 10}, [
        {link, {ls1mcs_link_sup, start_link, [LinkName]}, permanent, 5000, supervisor, [ls1mcs_link_sup]}
    ]}}.



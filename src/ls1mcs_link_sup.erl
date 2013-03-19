-module(ls1mcs_link_sup).
-behaviour(supervisor).
-export([start_link/1]).    % API
-export([init/1]).          % CB


%% =============================================================================
%%  API functions.
%% =============================================================================


%%
%% @doc Create this supervisor.
%%
start_link(Ls1pName) ->
    supervisor:start_link(?MODULE, {Ls1pName}).



%% =============================================================================
%%  Callbacks for supervisor.
%% =============================================================================


%%
%% @doc Supervisor initialization (CB).
%%
-spec init({tuple()}) -> tuple().
init({Ls1pName}) ->
    Rs232Name = {via, gproc, {n, l, ls1mcs_rs323}},
    KissName  = {via, gproc, {n, l, ls1mcs_kiss}},
    Ax25Name  = {via, gproc, {n, l, ls1mcs_ax25}},
    {ok, {{one_for_all, 100, 10}, [
        {rs232, {ls1mcs_rs323, start_link, [KissName]           }, permanent, 5000, worker, [ls1mcs_rs323]},
        {kiss,  {ls1mcs_kiss,  start_link, [Rs232Name, Ax25Name]}, permanent, 5000, worker, [ls1mcs_kiss] },
        {ax25,  {ls1mcs_ax25,  start_link, [KissName,  Ls1pName]}, permanent, 5000, worker, [ls1mcs_ax25] },
        {ls1p,  {ls1mcs_ls1p,  start_link, [           Ax25Name]}, permanent, 5000, worker, [ls1mcs_ls1p] }
    ]}}.



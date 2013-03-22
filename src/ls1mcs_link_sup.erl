-module(ls1mcs_link_sup).
-behaviour(supervisor).
-export([start_link/3]).    % API
-export([init/1]).          % CB


%% =============================================================================
%%  API functions.
%% =============================================================================


%%
%% @doc Create this supervisor.
%%
start_link(Ls1pName, IoDevice, InputLogFile) ->
    supervisor:start_link(?MODULE, {Ls1pName, IoDevice, InputLogFile}).



%% =============================================================================
%%  Callbacks for supervisor.
%% =============================================================================


%%
%% @doc Supervisor initialization (CB).
%%
-spec init({tuple()}) -> tuple().
init({Ls1pName, IoDevice, InputLogFile}) ->
    Rs232Name = {n, l, ls1mcs_proto_rs323},
    KissName  = {n, l, ls1mcs_proto_kiss},
    Ax25Name  = {n, l, ls1mcs_proto_ax25},
    Ls1pName  = {n, l, ls1mcs_proto_ls1p},
    {ok, {{one_for_all, 100, 10}, [
        {rs232, {ls1mcs_proto_rs323, start_link, [Rs232Name, KissName, IoDevice, InputLogFile]}, permanent, 5000, worker, [ls1mcs_proto_rs323]},
        {kiss,  {ls1mcs_proto_kiss,  start_link, [KissName, Rs232Name, Ax25Name]},               permanent, 5000, worker, [ls1mcs_proto_kiss] },
        {ax25,  {ls1mcs_proto_ax25,  start_link, [Ax25Name, KissName,  Ls1pName]},               permanent, 5000, worker, [ls1mcs_proto_ax25] },
        {ls1p,  {ls1mcs_proto_ls1p,  start_link, [Ls1pName, Ax25Name]},                          permanent, 5000, worker, [ls1mcs_proto_ls1p] }
    ]}}.



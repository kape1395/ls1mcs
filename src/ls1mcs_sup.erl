-module(ls1mcs_sup).
-behaviour(supervisor).
-export([start_link/6]).    % API
-export([init/1]).          % CB


%% =============================================================================
%%  API functions.
%% =============================================================================


%%
%% @doc Create this supervisor.
%%
start_link(TncDevice, LocalCall, RemoteCall, InputLog, OutputLog, StartProto) ->
    Args = {TncDevice, LocalCall, RemoteCall, InputLog, OutputLog, StartProto},
    supervisor:start_link(?MODULE, Args).



%% =============================================================================
%%  Callbacks for supervisor.
%% =============================================================================


%%
%% @doc Supervisor initialization (CB).
%%
init({TncDevice, LocalCall, RemoteCall, InputLog, _OutputLog, StartProto}) ->
    ConnMod = ls1mcs_connection,
    LSupMod = ls1mcs_link_sup,

    LinkRef = LSupMod:top_ref(),
    ConnName = {n, l, ConnMod},
    ConnRef  = ls1mcs_protocol:make_ref(ConnMod, ConnName),

    ConnArgs = [ConnName, LinkRef],
    LSupArgs = [ConnRef, TncDevice, InputLog, LocalCall, RemoteCall],

    case StartProto of
        false ->
            {ok, {{one_for_all, 100, 10}, []}};
        true ->
            {ok, {{one_for_all, 100, 10}, [
                {link, {LSupMod, start_link, LSupArgs}, permanent, 5000, supervisor, [LSupMod]},
                {conn, {ConnMod, start_link, ConnArgs}, permanent, 5000, worker,     [ConnMod]}
            ]}}
    end.



-module(ls1mcs_link_sup).
-behaviour(supervisor).
-export([start_link/5, top_ref/0]). % API
-export([init/1]). % CB

-define(LS1P_MOD,  ls1mcs_proto_ls1p).
-define(LS1P_NAME, {n, l, ?LS1P_MOD}).


%% =============================================================================
%%  API functions.
%% =============================================================================


%%
%% @doc Create this supervisor.
%%
start_link(ProtocolUserRef, IoDevice, InputLogFile, LocalCall, RemoteCall) ->
    Args = {ProtocolUserRef, IoDevice, InputLogFile, LocalCall, RemoteCall},
    supervisor:start_link(?MODULE, Args).


%%
%%  Returns a reference to the uppermost protocol.
%%
top_ref() ->
    ls1mcs_protocol:make_ref(?LS1P_MOD, ?LS1P_NAME).



%% =============================================================================
%%  Callbacks for supervisor.
%% =============================================================================


%%
%% @doc Supervisor initialization (CB).
%%
-spec init({tuple()}) -> tuple().
init({ProtocolUserRef, IoDevice, InputLogFile, LocalCall, RemoteCall}) ->
    Rs232Mod = ls1mcs_proto_rs232,
    KissMod  = ls1mcs_proto_kiss,
    Ax25Mod  = ls1mcs_proto_ax25,
    Ls1pMod  = ?LS1P_MOD,

    Rs232Name = {n, l, Rs232Mod},
    KissName  = {n, l, KissMod},
    Ax25Name  = {n, l, Ax25Mod},
    Ls1pName  = ?LS1P_NAME,

    Rs232Ref = ls1mcs_protocol:make_ref(Rs232Mod, Rs232Name),
    KissRef  = ls1mcs_protocol:make_ref(KissMod,  KissName),
    Ax25Ref  = ls1mcs_protocol:make_ref(Ax25Mod,  Ax25Name),
    Ls1pRef  = ls1mcs_protocol:make_ref(Ls1pMod,  Ls1pName),

    Rs232Args = [Rs232Name, KissRef, IoDevice, InputLogFile],
    KissArgs  = [KissName, Rs232Ref, Ax25Ref],
    Ax25Args  = [Ax25Name, KissRef,  Ls1pRef, LocalCall, RemoteCall],
    Ls1pArgs  = [Ls1pName, Ax25Ref, ProtocolUserRef],

    {ok, {{one_for_all, 100, 10}, [
        {rs232, {Rs232Mod, start_link, Rs232Args}, permanent, 5000, worker, [Rs232Mod]},
        {kiss,  {KissMod,  start_link, KissArgs }, permanent, 5000, worker, [KissMod] },
        {ax25,  {Ax25Mod,  start_link, Ax25Args }, permanent, 5000, worker, [Ax25Mod] },
        {ls1p,  {Ls1pMod,  start_link, Ls1pArgs }, permanent, 5000, worker, [Ls1pMod] }
    ]}}.



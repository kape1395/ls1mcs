-module(ls1mcs_link_sup).
-behaviour(supervisor).
-export([start_link/3, top_ref/0]). % API
-export([init/1]). % CB

-define(LS1P_MOD,  ls1mcs_proto_ls1p).
-define(LS1P_NAME, {n, l, ?LS1P_MOD}).


%% =============================================================================
%%  API functions.
%% =============================================================================


%%
%% @doc Create this supervisor.
%%
start_link(ConnRef, LinkType, LinkOptions) ->
    Args = {ConnRef, LinkType, LinkOptions},
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).


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
-spec init({
        ConnRef     :: tuple(),
        LinkType    :: atom(),
        LinkOptions :: list()
    }) -> tuple().

init({ConnRef, tnc_wa8ded_hostmode, LinkOptions}) ->
    Device    = proplists:get_value(device,     LinkOptions),
    LocalCall = proplists:get_value(local_call, LinkOptions),

    TncMod  = ls1mcs_tnc_wa8ded_hm,
    Ls1pMod = ?LS1P_MOD,

    TncName  = {n, l, TncMod},
    Ls1pName = ?LS1P_NAME,

    TncRef  = ls1mcs_protocol:make_ref(TncMod, TncName),
    Ls1pRef = ls1mcs_protocol:make_ref(Ls1pMod, Ls1pName),

    TncArgs  = [TncName, Ls1pRef, Device, LocalCall],
    Ls1pArgs = [Ls1pName, TncRef, ConnRef],

    {ok, {{one_for_all, 100, 10}, [
        {tnc,  {TncMod,  start_link, TncArgs }, permanent, 5000, worker, [TncMod]},
        {ls1p, {Ls1pMod, start_link, Ls1pArgs}, permanent, 5000, worker, [Ls1pMod]}
    ]}};

init({ConnRef, tnc_tapr_kiss, LinkOptions}) ->
    Device     = proplists:get_value(device,      LinkOptions),
    LocalCall  = proplists:get_value(local_call,  LinkOptions),
    RemoteCall = proplists:get_value(remote_call, LinkOptions),

    TaprMod = ls1mcs_tnc_tapr_kiss,
    KissMod = ls1mcs_proto_kiss,
    Ax25Mod = ls1mcs_proto_ax25,
    Ls1pMod = ?LS1P_MOD,

    TaprName = {n, l, TaprMod},
    KissName = {n, l, KissMod},
    Ax25Name = {n, l, Ax25Mod},
    Ls1pName = ?LS1P_NAME,

    TaprRef = ls1mcs_protocol:make_ref(TaprMod, TaprName),
    KissRef = ls1mcs_protocol:make_ref(KissMod, KissName),
    Ax25Ref = ls1mcs_protocol:make_ref(Ax25Mod, Ax25Name),
    Ls1pRef = ls1mcs_protocol:make_ref(Ls1pMod, Ls1pName),

    TaprArgs = [TaprName, KissRef, Device],
    KissArgs = [KissName, TaprRef, Ax25Ref],
    Ax25Args = [Ax25Name, KissRef, Ls1pRef, LocalCall, RemoteCall, tnc],
    Ls1pArgs = [Ls1pName, Ax25Ref, ConnRef],

    {ok, {{one_for_all, 100, 10}, [
        {sndm, {TaprMod, start_link, TaprArgs}, permanent, 5000, worker, [TaprMod]},
        {kiss, {KissMod, start_link, KissArgs}, permanent, 5000, worker, [KissMod]},
        {ax25, {Ax25Mod, start_link, Ax25Args}, permanent, 5000, worker, [Ax25Mod]},
        {ls1p, {Ls1pMod, start_link, Ls1pArgs}, permanent, 5000, worker, [Ls1pMod]}
    ]}};

init({ConnRef, soundmodem, LinkOptions}) ->
    Device     = proplists:get_value(device,      LinkOptions),
    LocalCall  = proplists:get_value(local_call,  LinkOptions),
    RemoteCall = proplists:get_value(remote_call, LinkOptions),

    SndmMod = ls1mcs_tnc_smodem,
    KissMod = ls1mcs_proto_kiss,
    Ax25Mod = ls1mcs_proto_ax25,
    Ls1pMod = ?LS1P_MOD,

    SndmName = {n, l, SndmMod},
    KissName = {n, l, KissMod},
    Ax25Name = {n, l, Ax25Mod},
    Ls1pName = ?LS1P_NAME,

    SndmRef = ls1mcs_protocol:make_ref(SndmMod, SndmName),
    KissRef = ls1mcs_protocol:make_ref(KissMod, KissName),
    Ax25Ref = ls1mcs_protocol:make_ref(Ax25Mod, Ax25Name),
    Ls1pRef = ls1mcs_protocol:make_ref(Ls1pMod, Ls1pName),

    SndmArgs = [SndmName, KissRef, Device],
    KissArgs = [KissName, SndmRef, Ax25Ref],
    Ax25Args = [Ax25Name, KissRef, Ls1pRef, LocalCall, RemoteCall, std],
    Ls1pArgs = [Ls1pName, Ax25Ref, ConnRef],

    {ok, {{one_for_all, 100, 10}, [
        {sndm, {SndmMod, start_link, SndmArgs}, permanent, 5000, worker, [SndmMod]},
        {kiss, {KissMod, start_link, KissArgs}, permanent, 5000, worker, [KissMod]},
        {ax25, {Ax25Mod, start_link, Ax25Args}, permanent, 5000, worker, [Ax25Mod]},
        {ls1p, {Ls1pMod, start_link, Ls1pArgs}, permanent, 5000, worker, [Ls1pMod]}
    ]}};

init({ConnRef, void, _LinkOptions}) ->
    VoidMod = ls1mcs_tnc_void,
    Ls1pMod = ?LS1P_MOD,

    VoidName = {n, l, VoidMod},
    Ls1pName = ?LS1P_NAME,

    VoidRef = ls1mcs_protocol:make_ref(VoidMod, VoidName),

    VoidArgs = [VoidName],
    Ls1pArgs = [Ls1pName, VoidRef, ConnRef],

    {ok, {{one_for_all, 100, 10}, [
        {void, {VoidMod, start_link, VoidArgs}, permanent, 5000, worker, [VoidMod]},
        {ls1p, {Ls1pMod, start_link, Ls1pArgs}, permanent, 5000, worker, [Ls1pMod]}
    ]}};

init({ConnRef, file, LinkOptions}) ->
    DataDir = proplists:get_value(data_dir, LinkOptions),

    FileMod = ls1mcs_tnc_file,
    Ls1pMod = ?LS1P_MOD,

    FileName = {n, l, FileMod},
    Ls1pName = ?LS1P_NAME,

    FileRef = ls1mcs_protocol:make_ref(FileMod, FileName),
    Ls1pRef = ls1mcs_protocol:make_ref(Ls1pMod, Ls1pName),

    FileArgs = [FileName, Ls1pRef, DataDir],
    Ls1pArgs = [Ls1pName, FileRef, ConnRef],

    {ok, {{one_for_all, 100, 10}, [
        {file, {FileMod, start_link, FileArgs}, permanent, 5000, worker, [FileMod]},
        {ls1p, {Ls1pMod, start_link, Ls1pArgs}, permanent, 5000, worker, [Ls1pMod]}
    ]}}.


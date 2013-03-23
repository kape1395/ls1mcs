-module(ls1mcs_app).
-behaviour(application).
-export([start/2, stop/1]).

-define(APP, ls1mcs).

%% =============================================================================
%%  Application callbacks
%% =============================================================================


%%  @doc
%%  Start the application.
%%
start(_StartType, _StartArgs) ->
    {ok, TncDevice}  = application:get_env(?APP, tnc_device),
    {ok, LocalCall}  = application:get_env(?APP, local_call),
    {ok, RemoteCall} = application:get_env(?APP, remote_call),
    {ok, InputLog}   = application:get_env(?APP, input_log),
    {ok, OutputLog}  = application:get_env(?APP, output_log),
    ls1mcs_sup:start_link(TncDevice, LocalCall, RemoteCall, InputLog, OutputLog).


%%  @doc
%%  Stop the application.
%%
stop(_State) ->
    ok.



%% =============================================================================
%%  Helper functions.
%% =============================================================================


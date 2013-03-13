-module(gen_ax25u).
-behaviour(gen_server).
-export([start/4, start_link/4, stop/1, info/1, send/2, recv/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



%% =============================================================================
%%  Public API
%% =============================================================================

start(Name, LocalPort, RemoteCall, Options) ->
    gen_server:start(Name, ?MODULE, {LocalPort, RemoteCall, Options}, []).

start_link(Name, LocalPort, RemoteCall, Options) ->
    gen_server:start_link(Name, ?MODULE, {LocalPort, RemoteCall, Options}, []).

stop(Name) ->
    gen_server:cast(Name, {stop}).

info(Name) ->
    gen_server:call(Name, {info}).

send(Name, Message) ->
    gen_server:cast(Name, {send, Message}).

recv(Name) ->
    gen_server:cast(Name, {recv}).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    port
}).



%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================


%%
%%  Gen server initialization. Creates the corresponding port.
%%
init({LocalPort, RemoteCall, Options}) ->
    process_flag(trap_exit, true),
    PortExec = "priv/gen_ax25u_port",
    case proplists:get_value(port_proxy, Options) of
        undefined ->
            PortName = {spawn_executable, PortExec},
            PortArgs = {args, [LocalPort, RemoteCall]};
        PortProxy ->
            PortName = {spawn_executable, PortProxy},
            PortArgs = {args, [PortExec, LocalPort, RemoteCall]}
    end,
    Port = erlang:open_port(PortName, [PortArgs, {packet, 2}, exit_status, use_stdio, binary]),
    {ok, #state{port = Port}}.


%%
%%  Synchronous calls.
%%
handle_call({info}, _From, State = #state{port = Port}) ->
    ok = send_port_msg(Port, {info, self()}),
    receive
        {Port, {data, Binary}} ->
            {reply, {ok, erlang:binary_to_term(Binary)}, State}
    after 10000 ->
            {stop, no_response_from_port, State}
    end.


%%
%%  Asynchronous calls.
%%
handle_cast({send, BinMsg}, State = #state{port = Port}) ->
    send_port_msg(Port, {send, BinMsg}),
    {noreply, State};

handle_cast({stop}, State = #state{port = Port}) ->
    ok = send_port_msg(Port, {stop}),
    {noreply, State}.


%%
%%  Messages from the port.
%%
handle_info({Port, {data, Binary}}, State = #state{port = Port}) ->
    Data = erlang:binary_to_term(Binary),
    error_logger:info_msg("Data from the port received: ~p~n", [Data]),
    {noreply, State};

handle_info({Port, {exit_status, 0}}, State = #state{port = Port}) ->
    {stop, normal, State#state{port = undefined}};

handle_info({Port, {exit_status, RC}}, State = #state{port = Port}) ->
    {stop, {port_exited, RC}, State#state{port = undefined}};

handle_info({'EXIT', Port, Reason}, State = #state{port = Port}) ->
    {stop, {port_terminated, Reason}, State#state{port = undefined}}.


%%
%%  Termination.
%%
terminate(Reason, #state{port = Port}) ->
    error_logger:info_msg("Terminating, reason=~p, port=~p~n", [Reason, Port]),
    ok.


%%
%%  Upgrades.
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% =============================================================================
%%  Internal Functions.
%% =============================================================================

send_port_msg(Port, Message) ->
    true = erlang:port_command(Port, erlang:term_to_binary(
        Message,
        [{minor_version, 1}]
    )),
    ok.

-module(gen_ax25u).
-behaviour(gen_server).
-export([start/3, start_link/3, stop/1, info/1, send/2, recv/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



%% =============================================================================
%%  Public API
%% =============================================================================

start(Name, LocalPort, RemoteCall) ->
    gen_server:start(Name, ?MODULE, {LocalPort, RemoteCall}, []).

start_link(Name, LocalPort, RemoteCall) ->
    gen_server:start_link(Name, ?MODULE, {LocalPort, RemoteCall}, []).

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
init({LocalPort, RemoteCall}) ->
    process_flag(trap_exit, true),
    Port = erlang:open_port({spawn_executable, "priv/gen_ax25u_port"}, [
        {packet, 2},
        {args, [LocalPort, RemoteCall]},
        exit_status,
        use_stdio,
        binary
    ]),
    {ok, #state{port = Port}}.


%%
%%  Synchronous calls.
%%
handle_call({info}, _From, State = #state{port = Port}) ->
    send_port_info(Port),
    receive
        {Port, {data, {info, LocalCall, RemoteCall}}} ->
            {reply, {ok, {LocalCall, RemoteCall}}, State}
    after 10000 ->
            {stop, no_response_from_port, State}
    end.


%%
%%  Asynchronous calls.
%%
handle_cast({stop}, State = #state{port = Port}) ->
    send_port_stop(Port),
    {noreply, State}.


%%
%%  Messages from the port.
%%
handle_info({Port, {data, Data}}, State = #state{port = Port}) ->
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

send_port_stop(Port) ->
    erlang:port_command(Port, erlang:term_to_binary(
        {stop},
        [{minor_version, 1}]
    )).

send_port_info(Port) ->
    erlang:port_command(Port, erlang:term_to_binary(
        {info, self()},
        [{minor_version, 1}]
    )).

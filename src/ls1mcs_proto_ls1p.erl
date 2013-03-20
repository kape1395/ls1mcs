-module(ls1mcs_proto_ls1p).
-behaviour(gen_server).
-behaviour(ls1mcs_protocol).
-export([start_link/3, send/2, received/2]).
-export([encode/1, decode/1]). % For tests.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("ls1mcs.hrl").
-include("ls1p.hrl").


-define(ADDR_MAP, [
    {arm,       0},
    {arduino,   1},
    {eps,       2},
    {gps,       3},
    {helium,    4}
]).
-define(PORT_MAP, [
    {arm,       ping,         0},
    {arm,       cmd_log,      1},
    {arm,       cmd_kill,     2},
    {arm,       tm_archive,   3},
    {arm,       tm_realtime,  4},
    {arm,       gps_log_bin,  5},
    {arm,       gps_log_nmea, 6},
    {arduino,   default,      0},
    {eps,       default,      0},
    {gps,       nmea,         0},
    {gps,       binary,       1},
    {helium,    default,      0}
]).
-define(ACK_MAP, [
    {false, 0},
    {true,  1}
]).


%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%
%%
start_link(Name, Lower, Upper) ->
    gen_server:start_link({via, gproc, Name}, ?MODULE, {Lower, Upper}, []).


%%
%%
%%
send(Ref, Data) when is_binary(Data) ->
    gen_server:cast({via, gproc, Ref}, {send, Data}).


%%
%%  Not used here.
%%
received(Ref, Data) when is_binary(Data) ->
    gen_server:cast({via, gproc, Ref}, {send, Data}).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    lower,      %% Lower protocol ref.
    upper       %% Upper protocol ref.
}).


%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================


%%
%%
%%
init({Lower, Upper}) ->
    self() ! {initialize},
    {ok, #state{lower = Lower, upper = Upper}}.


%%
%%
%%
handle_call(_Message, _From, State) ->
    {stop, not_implemented, State}.


%%
%%  TODO:
%%
handle_cast({send, _Data}, State = #state{}) ->
    {noreply, State};

handle_cast({received, _Data}, State = #state{}) ->
    {noreply, State}.


%%
%%  Deffered initialization.
%%
handle_info({initialize}, State = #state{lower = Lower, upper = Upper}) ->
    ls1mcs_protocol:await(Lower),
    ls1mcs_protocol:await(Upper),
    {noreply, State}.


%%
%%
%%
terminate(_Reason, _State) ->
    ok.


%%
%%
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% =============================================================================
%%  Internal Functions.
%% =============================================================================

%%
%%  LS1P encoder.
%%
encode(Frame) when is_record(Frame, ls1p_cmd_frame) ->
    #ls1p_cmd_frame{
        dest_addr = Addr, dest_port = Port, ack = Ack,
        cref = Cref, delay = Delay, data = Data
    } = Frame,
    AddrBin = encode_addr(Addr),
    PortBin = encode_port(Addr, Port),
    AckBin  = encode_ack(Ack),
    FrameBin = <<AddrBin:3, PortBin:4, AckBin:1, Cref:16, Delay:16, Data/binary>>,
    {ok, FrameBin}.

encode_addr(Addr) ->
    {Addr, AddrBin} = lists:keyfind(Addr, 1, ?ADDR_MAP),
    AddrBin.

encode_port(Addr, Port) ->
    FilterFun = fun ({A, P, _}) -> (A =:= Addr andalso P =:= Port) end,
    [{Addr, Port, PortBin}] = lists:filter(FilterFun, ?PORT_MAP),
    PortBin.

encode_ack(Ack) ->
    {Ack, AckBin} = lists:keyfind(Ack, 1, ?ACK_MAP),
    AckBin.


%%
%%  LS1P decoder.
%%
decode(FrameBin) ->
    <<AddrBin:3, PortBin:4, AckBin:1, Cref:16, Fragment:16, Data/binary>> = FrameBin,
    Addr = decode_addr(AddrBin),
    Port = decode_port(Addr, PortBin),
    Ack = decode_ack(AckBin),
    Frame = #ls1p_dat_frame{
        src_addr = Addr, src_port = Port, ack = Ack,
        cref = Cref, fragment = Fragment, data = Data
    },
    {ok, Frame}.

decode_addr(AddrBin) ->
    {Addr, AddrBin} = lists:keyfind(AddrBin, 2, ?ADDR_MAP),
    Addr.

decode_port(Addr, PortBin) ->
    FilterFun = fun ({A, _, P}) -> (A =:= Addr andalso P =:= PortBin) end,
    [{Addr, Port, PortBin}] = lists:filter(FilterFun, ?PORT_MAP),
    Port.

decode_ack(AckBin) ->
    {Ack, AckBin} = lists:keyfind(AckBin, 2, ?ACK_MAP),
    Ack.


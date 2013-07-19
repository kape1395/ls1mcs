-module(ls1mcs_proto_ls1p).
-compile([{parse_transform, lager_transform}]).
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
    {arm,       ping,       16#0},
    {arm,       kill,       16#1},
    {arm,       downlink,   16#2},
    {arm,       runtime_tm, 16#3},
    {arm,       job_period, 16#4},
    {arm,       multi,      16#F},
    {arduino,   take_photo, 16#0},
    {arduino,   photo_meta, 16#1},
    {arduino,   photo_data, 16#2},
    {eps,       command,    16#0},
    {gps,       nmea,       16#0},
    {gps,       binary,     16#1},
    {helium,    command,    16#0}
]).
-define(BOOL_MAP, [
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
send(Ref, Data) when is_record(Data, ls1p_cmd_frame) ->
    gen_server:cast({via, gproc, Ref}, {send, Data}).


%%
%%  Not used here.
%%
received(Ref, Data) when is_binary(Data) ->
    gen_server:cast({via, gproc, Ref}, {received, Data}).



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
%%
%%
handle_cast({send, Frame}, State = #state{lower = Lower}) ->
    {ok, DataBin} = encode(Frame),
    ok = ls1mcs_protocol:send(Lower, DataBin),
    {noreply, State};

handle_cast({received, DataBin}, State = #state{upper = Upper}) ->
    try decode(DataBin) of
        {ok, Frame} ->
            lager:debug("ls1mcs_proto_ls1p: Decoded frame: ~p from ~p", [Frame, DataBin]),
            ok = ls1mcs_protocol:send(Upper, Frame)
    catch
        ErrType:ErrCode ->
            lager:debug(
                "ls1mcs_proto_ls1p: Received invalid frame: ~p, error=~p:~p, trace=~p",
                [DataBin, ErrType, ErrCode, erlang:get_stacktrace()]
            )
    end,
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
    {Ack, AckBin} = lists:keyfind(Ack, 1, ?BOOL_MAP),
    AckBin.


%%
%%  LS1P decoder.
%%
decode(<<AddrBin:3, PortBin:4, StatusBin:1, Cref:16, RecvStatus:8>>) ->
    Addr = decode_addr(AddrBin),
    Port = decode_port(Addr, PortBin),
    Status = decode_se(StatusBin),
    Frame = #ls1p_ack_frame{
        src_addr = Addr, src_port = Port, status = Status,
        cref = Cref, recv_status = RecvStatus
    },
    {ok, Frame};

decode(<<AddrBin:3, PortBin:4, EofBin:1, Cref:16, Fragment:16, Data/binary>>) ->
    Addr = decode_addr(AddrBin),
    Port = decode_port(Addr, PortBin),
    Eof = decode_se(EofBin),
    Frame = #ls1p_dat_frame{
        src_addr = Addr, src_port = Port, eof = Eof,
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

decode_se(SEBin) ->
    {SE, SEBin} = lists:keyfind(SEBin, 2, ?BOOL_MAP),
    SE.


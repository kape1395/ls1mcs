-module(ls1mcs_proto_ls1p).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-behaviour(ls1mcs_protocol).
-export([start_link/3, send/2, received/2]).
-export([encode/1, decode/1]). % For tests.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("ls1mcs.hrl").
-include("ls1p.hrl").

-define(GROUND_ADDR, 2#111).
-define(GROUND_PORT_ACK,  16#0).
-define(GROUND_PORT_DATA, 16#1).
-define(GROUND_PORT_TM,   16#2).


-define(ADDR_MAP, [
    {arm,       0},
    {arduino,   1},
    {eps,       2},
    {gps,       3},
    {helium,    4},
    {ground,    ?GROUND_ADDR}
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
    {helium,    command,    16#0},
    {ground,    ack,        ?GROUND_PORT_ACK},
    {ground,    data,       ?GROUND_PORT_DATA},
    {ground,    telemetry,  ?GROUND_PORT_TM}
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
    ok = ls1mcs_store:add_ls1p_frame(Frame, DataBin, erlang:now()),
    ok = ls1mcs_protocol:send(Lower, DataBin),
    {noreply, State};

handle_cast({received, DataBin}, State = #state{upper = Upper}) ->
    try decode(DataBin) of
        {ok, Frame} ->
            lager:debug("ls1mcs_proto_ls1p: Decoded frame: ~p from ~p", [Frame, DataBin]),
            ok = ls1mcs_store:add_ls1p_frame(Frame, DataBin, erlang:now()),
            ok = ls1mcs_protocol:received(Upper, Frame)
    catch
        ErrType:ErrCode ->
            lager:debug(
                "ls1mcs_proto_ls1p: Received invalid frame: ~p, error=~p:~p, trace=~p",
                [DataBin, ErrType, ErrCode, erlang:get_stacktrace()]
            ),
            ok = ls1mcs_store:add_unknown_frame(DataBin, erlang:now())
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

%%  Acknowledgement frame.
encode(Frame) when is_record(Frame, ls1p_ack_frame) ->
    #ls1p_ack_frame{
        status = Status,
        cref = Cref,
        recv_status = RecvStatus
    } = Frame,
    StatusBin = encode_bool(Status),
    FrameBin = <<?GROUND_ADDR:3, ?GROUND_PORT_ACK:4, StatusBin:1, Cref:16, RecvStatus:8>>,
    {ok, FrameBin};

%%  Data frame.
encode(Frame) when is_record(Frame, ls1p_data_frame) ->
    #ls1p_data_frame{
        eof = Eof,
        cref = Cref,
        fragment = Fragment,
        data = Data
    } = Frame,
    EofBin = encode_bool(Eof),
    FrameBin = <<?GROUND_ADDR:3, ?GROUND_PORT_DATA:4, EofBin:1, Cref:16, Fragment:16, Data/binary>>,
    {ok, FrameBin};

%%  Telemetry frame.
encode(Frame) when is_record(Frame, ls1p_tm_frame) ->
    #ls1p_tm_frame{
        timestamp = Timestamp,
        data = Data
    } = Frame,
    FrameBin = <<?GROUND_ADDR:3, ?GROUND_PORT_TM:4, 0:1, Timestamp:16, Data/binary>>,
    {ok, FrameBin};

%%  Command frame.
encode(Frame) when is_record(Frame, ls1p_cmd_frame) ->
    #ls1p_cmd_frame{
        addr = Addr, port = Port, ack = Ack,
        cref = Cref, delay = Delay, data = Data
    } = Frame,
    AddrBin = encode_addr(Addr),
    PortBin = encode_port(Addr, Port),
    AckBin  = encode_bool(Ack),
    FrameBin = <<AddrBin:3, PortBin:4, AckBin:1, Cref:16, Delay:16, Data/binary>>,
    {ok, FrameBin}.




%%
%%  LS1P decoder.
%%

%%  Acknowledgement frame.
decode(<<?GROUND_ADDR:3, ?GROUND_PORT_ACK:4, StatusBin:1, Cref:16, RecvStatus:8>>) ->
    Status = decode_bool(StatusBin),
    Frame = #ls1p_ack_frame{
        status = Status,
        cref = Cref,
        recv_status = RecvStatus
    },
    {ok, Frame};

%%  Data frame
decode(<<?GROUND_ADDR:3, ?GROUND_PORT_DATA:4, EofBin:1, Cref:16, Fragment:16, Data/binary>>) ->
    Eof = decode_bool(EofBin),
    Frame = #ls1p_data_frame{
        eof = Eof,
        cref = Cref,
        fragment = Fragment,
        data = Data
    },
    {ok, Frame};

%%  Telemetry frame
decode(<<?GROUND_ADDR:3, ?GROUND_PORT_TM:4, 0:1, Timestamp:4/binary, Data/binary>>) ->
    << % 1 + 4 + 43 + (6 + 14 + 14) * 5 + 16.
        EPS:43/binary,
        HE:16/binary,
        IS_0_HMC:6/binary, IS_0_L3GD20:14/binary, IS_0_MPU:14/binary,
        IS_1_HMC:6/binary, IS_1_L3GD20:14/binary, IS_1_MPU:14/binary,
        IS_2_HMC:6/binary, IS_2_L3GD20:14/binary, IS_2_MPU:14/binary,
        IS_3_HMC:6/binary, IS_3_L3GD20:14/binary, IS_3_MPU:14/binary,
        IS_4_HMC:6/binary, IS_4_L3GD20:14/binary, IS_4_MPU:14/binary
    >> = Data,

    Frame = #ls1p_tm_frame{
        timestamp = Timestamp,
        data = Data
    },
    {ok, Frame};

%%  Command frame
decode(<<AddrBin:3, PortBin:4, AckBin:1, Cref:16, Delay:16, Data/binary>>) ->
    Addr = decode_addr(AddrBin),
    Port = decode_port(Addr, PortBin),
    Ack = decode_bool(AckBin),
    Frame = #ls1p_cmd_frame{
        addr = Addr,
        port = Port,
        ack = Ack,
        cref = Cref,
        delay = Delay,
        data = Data
    },
    {ok, Frame}.


%%
%%  Address encoding/decoding.
%%
encode_addr(Addr) ->
    {Addr, AddrBin} = lists:keyfind(Addr, 1, ?ADDR_MAP),
    AddrBin.


decode_addr(AddrBin) ->
    {Addr, AddrBin} = lists:keyfind(AddrBin, 2, ?ADDR_MAP),
    Addr.


%%
%%  Port encoding/decoding.
%%
encode_port(Addr, Port) ->
    FilterFun = fun ({A, P, _}) -> (A =:= Addr andalso P =:= Port) end,
    [{Addr, Port, PortBin}] = lists:filter(FilterFun, ?PORT_MAP),
    PortBin.

decode_port(Addr, PortBin) ->
    FilterFun = fun ({A, _, P}) -> (A =:= Addr andalso P =:= PortBin) end,
    [{Addr, Port, PortBin}] = lists:filter(FilterFun, ?PORT_MAP),
    Port.


%%
%%  Boolean encoding/decoding.
%%
encode_bool(false) -> 0;
encode_bool(true) -> 1.

decode_bool(0) -> false;
decode_bool(1) -> true.




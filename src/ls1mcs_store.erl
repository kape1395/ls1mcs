%%
%%  Main persistent store for the LS1MCS.
%%
-module(ls1mcs_store).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0, is_installed/0, install/0, install/1, wait_for_tables/1]).
-export([
    next_cref/0,
    add_ls1p_frame/3,
    add_unknown_frame/2
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include_lib("stdlib/include/qlc.hrl").
-include("ls1mcs.hrl").
-include("ls1p.hrl").

-define(ATTRS(R), {attributes, record_info(fields, R)}).


%% =============================================================================
%%  API Function Definitions
%% =============================================================================


%%
%%  Starts the store.
%%
-spec start_link() -> {ok, pid()} | term().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).


%%
%%  Checks, if the DB is installed.
%%
is_installed() ->
   try mnesia:table_info(ls1mcs_store_counter, all) of
        _ ->
            true
    catch
        _Type:_Error ->
            false
    end.


%%
%%  Creates the store schema.
%%
-spec install([atom()]) -> ok.
install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    create_tables(Nodes),
    rpc:multicall(Nodes, application, stop, [mnesia]),
    ok.


%%
%%  Creates the store schema on the current host only.
%%
-spec install() -> ok.
install() ->
    install([node()]).


%%
%%  Waits for all tables.
%%
-spec wait_for_tables(number()) -> ok | term().
wait_for_tables(Timeout) ->
    Tables = [
        ls1mcs_store_ls1p_sent,
        ls1mcs_store_ls1p_recv,
        ls1mcs_store_ls1p_tm,
        ls1mcs_store_ls1p_unkn,
        ls1mcs_store_counter
    ],
    mnesia:wait_for_tables(Tables, Timeout).



%% =============================================================================
%%  Table definitions.
%% =============================================================================

%%
%%  LS1P commands sent to the SAT.
%%
-record(ls1mcs_store_ls1p_sent, {
    cref,
    frame,
    bytes,
    sent_time
}).

%%
%%  LS1P responses (ACKs and DATA frames) received from the SAT.
%%
-record(ls1mcs_store_ls1p_recv, {
    cref,
    frame,
    bytes,
    recv_time
}).

%%
%%  Received LS1P telemetry frames.
%%
-record(ls1mcs_store_ls1p_tm, {
    recv_time,
    frame,
    bytes
}).

%%
%%  Unrecognized received LS1P frames.
%%
-record(ls1mcs_store_ls1p_unkn, {
    recv_time,
    bytes
}).

%%
%%  Misc counters.
%%
-record(ls1mcs_store_counter, {
    key,
    value
}).


%%
%%  Creates mnesia tables.
%%
create_tables(Nodes) ->
    DefOptDC = {disc_copies, Nodes},
    OK = {atomic, ok},
    OK = mnesia:create_table(ls1mcs_store_ls1p_sent, [{type, set}, ?ATTRS(ls1mcs_store_ls1p_sent), DefOptDC]),
    OK = mnesia:create_table(ls1mcs_store_ls1p_recv, [{type, bag}, ?ATTRS(ls1mcs_store_ls1p_recv), DefOptDC]),
    OK = mnesia:create_table(ls1mcs_store_ls1p_tm,   [{type, bag}, ?ATTRS(ls1mcs_store_ls1p_tm),   DefOptDC]),
    OK = mnesia:create_table(ls1mcs_store_ls1p_unkn, [{type, bag}, ?ATTRS(ls1mcs_store_ls1p_unkn), DefOptDC]),
    OK = mnesia:create_table(ls1mcs_store_counter,   [{type, set}, ?ATTRS(ls1mcs_store_counter),   DefOptDC]),
    ok.



%% =============================================================================
%%  Query functions
%% =============================================================================


%%
%%  Generates new CRef.
%%
next_cref() ->
    Next = mnesia:dirty_update_counter(ls1mcs_store_counter, cref, 1),
    CRef = Next rem 16#FFFF,
    Epoch = Next div 16#FFFF,
    {ok, Epoch, CRef}.


%%
%%  Add LS1P frame.
%%
add_ls1p_frame(Frame = #ls1p_cmd_frame{cref = CRef}, Bytes, Timestamp) ->
    Activity = fun () ->
        ok = mnesia:write(#ls1mcs_store_ls1p_sent{
            cref = CRef,
            frame = Frame,
            bytes = Bytes,
            sent_time = Timestamp
        }),
        ok
    end,
    mnesia:activity(transaction, Activity);

add_ls1p_frame(Frame = #ls1p_ack_frame{cref = CRef}, Bytes, Timestamp) ->
    Activity = fun () ->
        ok = mnesia:write(#ls1mcs_store_ls1p_recv{
            cref = CRef,
            frame = Frame,
            bytes = Bytes,
            recv_time = Timestamp
        }),
        ok
    end,
    mnesia:activity(transaction, Activity);

add_ls1p_frame(Frame = #ls1p_data_frame{cref = CRef}, Bytes, Timestamp) ->
    Activity = fun () ->
        ok = mnesia:write(#ls1mcs_store_ls1p_recv{
            cref = CRef,
            frame = Frame,
            bytes = Bytes,
            recv_time = Timestamp
        }),
        ok
    end,
    mnesia:activity(transaction, Activity);

add_ls1p_frame(Frame = #ls1p_tm_frame{}, Bytes, Timestamp) ->
    Activity = fun () ->
        ok = mnesia:write(#ls1mcs_store_ls1p_tm{
            frame = Frame,
            bytes = Bytes,
            recv_time = Timestamp
        }),
        ok
    end,
    mnesia:activity(transaction, Activity).


%%
%%  Add unknown frame (not recognized as a valid LS1P frame).
%%
add_unknown_frame(Bytes, Timestamp) ->
    Activity = fun () ->
        ok = mnesia:write(#ls1mcs_store_ls1p_unkn{
            bytes = Bytes,
            recv_time = Timestamp
        }),
        ok
    end,
    mnesia:activity(transaction, Activity).


%% =============================================================================
%%  Internal state of this module.
%% =============================================================================


-record(state, {
}).



%% =============================================================================
%%  Callbacks for gen_server (unused).
%% =============================================================================


init({}) ->
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {reply, undefined, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% =============================================================================
%%  Helper functions.
%% =============================================================================



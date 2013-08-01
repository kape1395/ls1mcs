%%
%%  For various tests and debugging.
%%
-module(ls1mcs_utl_test).
-compile([{parse_transform, lager_transform}]).
-export([start/0]).
-export([
    send_ping/0,
    send_take_photo/1,
    send_photo_meta/0,
    send_photo_data/3,
    load_photo/5,
    load_photo_frames/3
]).
-include("ls1mcs.hrl").


%% =============================================================================
%%  API Function Definitions
%% =============================================================================


%%
%%  Starts the LS1MCS application and its dependencies
%%  (to be used for development or debugging purposes only).
%%
start() ->
    ok = start_lager(),
    ok = start_missing_app(sasl),
    ok = start_missing_app(gproc),
    ok = start_mnesia(),
    ok = start_yaws(),
    ok = start_uart(),
    ok = start_missing_app(ls1mcs),
    ok.


%%
%%
%%
send_ping() ->
    {ok, _CRef} = ls1mcs_sat_link:send(#ls1p_cmd_frame{
        addr = arm,
        port = ping
    }).


%%
%%
%%
send_take_photo(Delay) ->
    Resolution = 0,
    {ok, _CRef} = ls1mcs_sat_link:send(#ls1p_cmd_frame{
        addr = arduino,
        port = take_photo,
        ack = true,
        delay = Delay,
        data = <<Resolution:8>>
    }).


%%
%%
%%
send_photo_meta() ->
    {ok, _CRef} = ls1mcs_sat_link:send(#ls1p_cmd_frame{
        addr = arduino,
        port = photo_meta
    }).


%%
%%
%%
send_photo_data(Size, From, Till) ->
    {ok, _CRef} = ls1mcs_sat_link:send(#ls1p_cmd_frame{
        addr = arduino,
        port = photo_data,
        ack = false,
        data = <<Size:8, From:16, Till:16>>
    }).


%%
%%  Load a photo from file to DB.
%%
load_photo(CRef, Size, From, Till, PhotoFile) ->
    {ok, PhotoBin} = file:read_file(PhotoFile),
    load_photo_bin(CRef, Size, 0, Till - From, PhotoBin).

load_photo_bin(_CRef, _Size, Index, Count, _Data) when Index >= Count ->
    ok;

load_photo_bin(CRef, Size, Index, Count, Data) ->
    case Data of
        <<Block:Size/binary, Tail/binary>> when size(Tail) > 0 ->
            Frame = #ls1p_data_frame{eof = false, cref = CRef, fragment = Index, data = Block},
            ok = ls1mcs_store:add_ls1p_frame(Frame, <<>>, erlang:now()),
            load_photo_bin(CRef, Size, Index + 1, Count, Tail);
        End ->
            Frame = #ls1p_data_frame{eof = true, cref = CRef, fragment = Index, data = End},
            ok = ls1mcs_store:add_ls1p_frame(Frame, <<>>, erlang:now())
    end.


%%
%%  Load fixed-length binary frames file and send it to the LS1P protocol handler.
%%
load_photo_frames(FramesFile, FrameLen, LSRef) when is_list(FramesFile) ->
    {ok, FramesBin} = file:read_file(FramesFile),
    case FramesBin of
        <<?GROUND_ADDR:3, ?GROUND_PORT_DATA:4, _Flag:1, CRef:16, _/binary>> ->
            lager:debug("First frame is DATA frame with cref=~p", [CRef]),
            load_photo_frames(FramesBin, FrameLen, LSRef);
        _->
            load_photo_frames(FramesBin, FrameLen, LSRef)
        %<<?GROUND_ADDR:3, ?GROUND_PORT_ACK:4, _Flag:1, _/binary>> ->
        %    lager:debug("First frame is ACK. Rejecting."),
        %    {error, acks};
        %<<?GROUND_ADDR:3, ?GROUND_PORT_TM:4, _Flag:1, _/binary>> ->
        %    lager:debug("First frame is TM frame. Rejecting.")
    end;

load_photo_frames(FramesBin, FrameLen, LSRef) ->
    case FramesBin of
        <<>> ->
            ok;
        <<FrameBin:FrameLen/binary, Tail/binary>> ->
            ok = load_photo_frames_received(FrameBin, LSRef),
            load_photo_frames(Tail, FrameLen, LSRef);
        LastFrameBin ->
            ok = load_photo_frames_received(LastFrameBin, LSRef),
            ok
    end.

load_photo_frames_received(<<>>, _LSRef) ->
    lager:error("empty frame");

load_photo_frames_received(FrameBin, LSRef) ->
    <<_BadHdr:8, FrameTail/binary>> = FrameBin,
    FixedFrame = <<?GROUND_ADDR:3, ?GROUND_PORT_DATA:4, 0:1, FrameTail/binary>>,
    ok = ls1mcs_protocol:received(LSRef, FixedFrame).



%% =============================================================================
%%  Helper functions.
%% =============================================================================


%%
%%
%%
start_lager() ->
    ok = start_missing_app(compiler),
    ok = start_missing_app(syntax_tools),
    ok = start_missing_app(lager).


%%
%%
%%
start_yaws() ->
    ok = start_missing_app(asn1),
    ok = start_missing_app(crypto),
    ok = start_missing_app(public_key),
    ok = start_missing_app(ssl),
    ok = start_missing_app(yaws),
    ok = start_missing_app(jiffy).


%%
%%
%%
start_uart() ->
    ok = start_missing_app(dthread),
    ok = start_missing_app(uart).


%%
%%
%%
start_mnesia() ->
    ok = start_missing_app(mnesia),
    case ls1mcs_store:is_installed() of
        true ->
            lager:info("Using existing mnesia DB.");
        false ->
            application:stop(mnesia),
            ls1mcs_store:install(),
            ok = start_missing_app(mnesia),
            ok = ls1mcs_store:wait_for_tables(60000),
            lager:info("New mnesia DB created on this node.")
    end,
    ok.


%%
%%
%%
start_missing_app(App) ->
    case application:start(App) of
        ok -> ok;
        {error,{already_started,App}} -> ok
    end.



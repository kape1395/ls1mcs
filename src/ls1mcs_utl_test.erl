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
    send_photo_data/2
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
    {ok, _CRef} = ls1mcs_connection:send(#ls1p_cmd_frame{
        addr = arm,
        port = ping
    }).


%%
%%
%%
send_take_photo(Delay) ->
    Resolution = 0,
    {ok, _CRef} = ls1mcs_connection:send(#ls1p_cmd_frame{
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
    {ok, _CRef} = ls1mcs_connection:send(#ls1p_cmd_frame{
        addr = arduino,
        port = photo_meta
    }).


%%
%%
%%
send_photo_data(From, Till) ->
    {ok, _CRef} = ls1mcs_connection:send(#ls1p_cmd_frame{
        addr = arduino,
        port = photo_data,
        ack = false,
        data = <<From:16, Till:16>>
    }).



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



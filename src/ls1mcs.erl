-module(ls1mcs).
-export([start/0, stop/0]).

%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%  Starts application (manually).
%%
start() ->
    ok = start_app(compiler),
    ok = start_app(syntax_tools),
    ok = start_app(lager),
    ok = start_app(sasl),
    ok = start_app(gproc),
    ok = start_app(dthread),
    ok = start_app(uart),
    ok = start_app(crypto),
    ok = start_app(asn1),
    ok = start_app(public_key),
    ok = start_app(ssl),
    ok = start_app(yaws),
    ok = start_app(ls1mcs).


stop() ->
    ok = stop_app(ls1mcs),
    ok = stop_app(yaws),
    ok = stop_app(ssl),
    ok = stop_app(public_key),
    ok = stop_app(asn1),
    ok = stop_app(crypto),
    ok = stop_app(uart),
    ok = stop_app(dthread),
    ok = stop_app(gproc),
    ok = stop_app(sasl),
    ok = stop_app(lager),
    ok = stop_app(syntax_tools),
    ok = stop_app(compiler).


%% =============================================================================
%%  Internal Functions.
%% =============================================================================


start_app(App) ->
    case application:start(App) of
        ok -> ok;
        {error,{already_started,App}} -> ok
    end.


stop_app(App) ->
    case application:stop(App) of
        ok -> ok;
        {error,{not_started,mnesia}} -> ok
    end.


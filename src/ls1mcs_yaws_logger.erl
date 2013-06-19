-module(ls1mcs_yaws_logger).
-behaviour(yaws_logger).
-compile([{parse_transform, lager_transform}]).
-export([open_log/3, close_log/3, wrap_log/4, write_log/4]).
-export([crashmsg/3]).
-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").


%% =============================================================================
%%  Callbacks for yaws_logger
%% =============================================================================

open_log(_ServerName, _Type, _LogDir) ->
    {true, {}}.

close_log(_ServerName, _Type, _State) ->
    ok.


wrap_log(_ServerName, _Type, State, _LogWrapSize) ->
    State.


write_log(_ServerName, _Type, _State, _Infos) ->
    ok.

%% =============================================================================
%%  Callbacks for errormod_crash
%% =============================================================================

crashmsg(Arg, _SC, L) ->
    Url = yaws_api:request_url(Arg),
    Method = yaws_api:http_request_method(Arg#arg.req),
    lager:error("ls1mcs_yaws_logger: crashmsg: method=~p url=~p error:~s", [Method, Url, L]),
    {content, "text/plain", L}.


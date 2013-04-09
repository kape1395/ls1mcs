-module(ls1mcs_yaws_logger).
-behaviour(yaws_logger).
-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-export([open_log/3, close_log/3, wrap_log/4, write_log/4]).

open_log(_ServerName, _Type, _LogDir) ->
    {true, {}}.

close_log(_ServerName, _Type, _State) ->
    ok.


wrap_log(_ServerName, _Type, State, _LogWrapSize) ->
    State.


write_log(_ServerName, _Type, _State, _Infos) ->
    ok.


%/--------------------------------------------------------------------
%| Copyright 2013-2014 Karolis Petrauskas
%|
%| Licensed under the Apache License, Version 2.0 (the "License");
%| you may not use this file except in compliance with the License.
%| You may obtain a copy of the License at
%|
%|     http://www.apache.org/licenses/LICENSE-2.0
%|
%| Unless required by applicable law or agreed to in writing, software
%| distributed under the License is distributed on an "AS IS" BASIS,
%| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%| See the License for the specific language governing permissions and
%| limitations under the License.
%\--------------------------------------------------------------------

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


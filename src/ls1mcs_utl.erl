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

%%
%%  Utility functions.
%%
-module(ls1mcs_utl).
-export([parse_tstamp/1, local_now/1, local_now/0]).

-define(UNIX_BIRTH, 62167219200).
-define(MEGA_SECS, 1000000).


%%
%%  Decode timestamp.
%%  calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).
%%  Eg. "2013-07-21T23:57:26", "2013-07-21 23:57:26", "2013-07-21".
%%
parse_tstamp(TStamp) when is_list(TStamp) ->
    parse_tstamp(erlang:list_to_binary(TStamp));

parse_tstamp(undefined) ->
    undefined;

parse_tstamp(DateBin) ->
    case DateBin of
        <<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary>> ->
            Hour = <<"00">>, Min = <<"00">>, Sec = <<"00">>, ok;
        <<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary, " ", Hour:2/binary, ":", Min:2/binary>> ->
            Sec = <<"00">>, ok;
        <<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary, " ", Hour:2/binary, ":", Min:2/binary, ":", Sec:2/binary>> ->
            ok;
        <<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary, "T", Hour:2/binary, ":", Min:2/binary, ":", Sec:2/binary>> ->
            ok
    end,
    Date = {
        {binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)},
        {binary_to_integer(Hour), binary_to_integer(Min), binary_to_integer(Sec)}
    },
    DateSecs = calendar:datetime_to_gregorian_seconds(Date) - ?UNIX_BIRTH,
    {DateSecs div ?MEGA_SECS, DateSecs rem ?MEGA_SECS, 0}.


%%
%%  Returns timestamp in local time.
%%
%%  NOTE: `os:timestamp()` and `erlang:now()` returns UTC.
%%
local_now({_, _, USec} = Now) ->
    Date = calendar:now_to_local_time(Now),
    DateSecs = calendar:datetime_to_gregorian_seconds(Date) - ?UNIX_BIRTH,
    {DateSecs div ?MEGA_SECS, DateSecs rem ?MEGA_SECS, USec}.

local_now() ->
    local_now(os:timestamp()).



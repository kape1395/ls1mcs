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
%%  Implementation of the KISS protocol.
%%  See http://www.ax25.net/kiss.aspx for more details.
%%
-module(ls1mcs_proto_kiss).
-behaviour(ls1mcs_proto).
-compile([{parse_transform, lager_transform}]).
-export([make_ref/0]).
-export([init/1, send/2, recv/2]).

-define(FEND,   16#C0).
-define(FESC,   16#DB).
-define(TFEND,  16#DC).
-define(TFESC,  16#DD).
-define(FT_DATA,        0). % Frame type = data frame, port number =  0.
-define(FT_TXDELAY,     1).
-define(FT_P,           2).
-define(FT_SlotTime,    3).
-define(FT_TXtail,      4).
-define(FT_FullDuplex,  5).
-define(FT_SetHardware, 6).
-define(FT_TIME,        9). % Non-standard frame, contains timestamp of the following data frame.

%%
%% calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).
%%
-define(UNIX_BIRTH, 62167219200).
-define(MEGA_SECS, 1000000).


%% =============================================================================
%%  Public API
%% =============================================================================

%%
%%
%%
make_ref() ->
    ls1mcs_proto:make_ref(?MODULE, {}).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    name,       %% Current state name.
    type,       %% Type of the current frame: data | time
    part,       %% Partly decoded frame.
    time,       %% Last date, as specified in a non-standard 09 frame.
    frames      %% Decoded frames.
}).



%% =============================================================================
%%  Callbacks for `ls1mcs_proto`.
%% =============================================================================

%%
%%
%%
init({}) ->
    {ok, init()}.


%%
%%
%%
send({Hdrs, Frame}, State) when is_binary(Frame) ->
    EncodedFrame = encode(Frame),
    {ok, [{Hdrs, EncodedFrame}], State}.


%%
%%
%%
recv({Hdrs, Body}, State) when is_binary(Body) ->
    {ok, Frames, NewState} = decode(binary_to_list(Body), State),
    {ok, [ {F ++ Hdrs, B} || {F, B} <- Frames ], NewState}.



%% =============================================================================
%%  Internal Functions.
%% =============================================================================

%%
%%  Initializes the state.
%%
init() ->
    #state{
        name = idle,
        type = undefined,
        part = [],
        time = undefined,
        frames = []
    }.


%%
%%  KISS escaping.
%%
escape_byte(?FEND) -> [?FESC, ?TFEND];
escape_byte(?FESC) -> [?FESC, ?TFESC];
escape_byte(Byte) -> Byte.


%%
%%  Encode data into a single KISS frame.
%%
encode(Data) ->
    InitialList = binary_to_list(Data),
    EncodedList = [?FEND, ?FT_DATA, lists:map(fun escape_byte/1, InitialList), ?FEND],
    list_to_binary(EncodedList).


%%
%%  Decode stream of bytes to KISS frames.
%%
decode(Data, State) ->
    NewState = #state{frames = Frames} = lists:foldl(fun decode_byte/2, State#state{frames = []}, Data),
    {ok, lists:reverse(Frames), NewState#state{frames = []}}.

decode_byte(?FEND, State = #state{name = idle}) -> State#state{name = frame_start};
decode_byte(_Byte, State = #state{name = idle}) -> State#state{name = idle};

decode_byte(?FT_DATA, State = #state{name = frame_start}) -> State#state{name = frame_data, type = data, part = []};
decode_byte(?FT_TIME, State = #state{name = frame_start}) -> State#state{name = frame_data, type = time, part = []};
decode_byte(?FEND,    State = #state{name = frame_start}) -> State#state{name = frame_start};
decode_byte(_Byte,    State = #state{name = frame_start}) -> State#state{name = idle};

decode_byte(?FEND, State = #state{name = frame_data})              -> frame_decoded(State);
decode_byte(?FESC, State = #state{name = frame_data})              -> State#state{name = frame_esc};
decode_byte(Byte,  State = #state{name = frame_data, part = Part}) -> State#state{part = [Byte | Part]};

decode_byte(?TFESC, State = #state{name = frame_esc, part = Part}) -> State#state{name = frame_data, part = [?FESC | Part]};
decode_byte(?TFEND, State = #state{name = frame_esc, part = Part}) -> State#state{name = frame_data, part = [?FEND | Part]};
decode_byte(Byte,   State = #state{name = frame_esc, part = Part}) -> State#state{name = frame_data, part = [Byte  | Part]}.

frame_decoded(State = #state{type = data, frames = Frames, part = Part, time = Time}) ->
    Frame = {
        [{time, Time}],
        list_to_binary(lists:reverse(Part))
    },
    State#state{
        name = frame_start,
        frames = [Frame | Frames],
        type = undefined,
        part = [],
        time = undefined
    };
frame_decoded(State = #state{type = time, part = Part}) ->
    %%  Example date: <<"2014-04-14 17:07:39.750 UTC">>
    %%  Example date: <<"2014-04-14 17:07:39.750 UTC;77,7;5,0;97;39,09°E;51,63°N">>
    TimeBin = list_to_binary(lists:reverse(Part)),
    Time = case TimeBin of
        <<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary, Sep:1/binary,
          Hour:2/binary, ":", Min:2/binary,   ":", Sec:2/binary, Rest/binary>>
          when Sep =:= <<" ">>; Sep =:= <<"T">>
          ->
            Date = {
                {binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)},
                {binary_to_integer(Hour), binary_to_integer(Min),   binary_to_integer(Sec)}
            },
            DateSecs = calendar:datetime_to_gregorian_seconds(Date) - ?UNIX_BIRTH,
            [SubSecsWithZone | _] = binary:split(Rest, <<";">>),
            case SubSecsWithZone of
                <<".", MSec:3/binary, Zone/binary>> when Zone =:= <<"">>; Zone =:= <<"Z">>; Zone =:= <<" UTC">> ->
                    {DateSecs div ?MEGA_SECS, DateSecs rem ?MEGA_SECS, binary_to_integer(MSec) * 1000};
                <<".", USec:6/binary, Zone/binary>> when Zone =:= <<"">>; Zone =:= <<"Z">>; Zone =:= <<" UTC">> ->
                    {DateSecs div ?MEGA_SECS, DateSecs rem ?MEGA_SECS, binary_to_integer(USec)};
                _ ->
                    lager:warning("Ignoring unknown time zone in date: ~p", [TimeBin]),
                    {DateSecs div ?MEGA_SECS, DateSecs rem ?MEGA_SECS, 0}
            end;
        _ ->
            lager:warning("Ignoring unknown timestamp: ~p", [TimeBin]),
            undefined
    end,
    State#state{
        name = frame_start,
        type = undefined,
        part = [],
        time = Time
    }.



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
-export([make_ref/0, preview/1]).
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


%% =============================================================================
%%  Public API
%% =============================================================================

%%
%%
%%
make_ref() ->
    ls1mcs_proto:make_ref(?MODULE, {}).


%%
%%  Returns decoded frames.
%%  Used for TM preview.
%%
preview(Data) when is_binary(Data) ->
    preview(binary_to_list(Data));

preview(Data) when is_list(Data) ->
    {ok, Frames, _State} = decode(Data, {idle, []}),
    {ok, Frames}.



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    name,
    buff
}).



%% =============================================================================
%%  Callbacks for `ls1mcs_proto`.
%% =============================================================================

%%
%%
%%
init({}) ->
    {ok, #state{name = idle, buff = []}}.


%%
%%
%%
send(Frame, State) when is_binary(Frame) ->
    EncodedFrame = encode(Frame),
    {ok, State, [EncodedFrame]}.


%%
%%
%%
recv(Frame, State = #state{name = Name, buff = Buff}) when is_binary(Frame) ->
    {ok, DecodedFrames, {NewName, NewBuff}} = decode(binary_to_list(Frame), {Name, Buff}),
    {ok, State#state{name = NewName, buff = NewBuff}, DecodedFrames}.



%% =============================================================================
%%  Internal Functions.
%% =============================================================================

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
decode(Data, {InitState, InitPartial}) ->
    {State, Decoded, Partial} = lists:foldl(fun decode_fun/2, {InitState, [], InitPartial}, Data),
    Frames = lists:reverse([ list_to_binary(lists:reverse(D)) || D <- Decoded]),
    {ok, Frames, {State, Partial}}.

decode_fun(?FEND,    {idle,        Decoded, _Partial}) -> {frame_start, Decoded,              []};
decode_fun(_Byte,    {idle,        Decoded, _Partial}) -> {idle,        Decoded,              []};
decode_fun(?FT_DATA, {frame_start, Decoded, _Partial}) -> {frame_data,  Decoded,              []};
decode_fun(?FEND,    {frame_start, Decoded, _Partial}) -> {frame_start, Decoded,              []};
decode_fun(_Byte,    {frame_start, Decoded, _Partial}) -> {idle,        Decoded,              []};
decode_fun(?FESC,    {frame_data,  Decoded, Partial})  -> {frame_esc,   Decoded,              Partial};
decode_fun(?FEND,    {frame_data,  Decoded, Partial})  -> {frame_start, [Partial | Decoded],  []};
decode_fun(Byte,     {frame_data,  Decoded, Partial})  -> {frame_data,  Decoded,              [Byte  | Partial]};
decode_fun(?TFESC,   {frame_esc,   Decoded, Partial})  -> {frame_data,  Decoded,              [?FESC | Partial]};
decode_fun(?TFEND,   {frame_esc,   Decoded, Partial})  -> {frame_data,  Decoded,              [?FEND | Partial]};
decode_fun(Byte,     {frame_esc,   Decoded, Partial})  -> {frame_data,  Decoded,              [Byte  | Partial]}.



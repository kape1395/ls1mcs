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

-module(ls1mcs_utl_cksum).
-export([checksum/2]).

%%
%%  Calculates hash'es / checksums.
%%
checksum(Data, fletcher8bit) ->
    {A, B} = lists:foldl(fun fletcher_fold/2, {0, 0}, erlang:binary_to_list(Data)),
    {ok, <<A:8, B:8>>}.



%%
%%  The 8-bit Fletcher algorithm (see RFC 1145 which describes TCP)
%%  is used to calculate the checksums for the Helium-100.
%%
fletcher_fold(Byte, {A, B}) ->
    NewA = 16#FF band (A + Byte),
    NewB = 16#FF band (B + NewA),
    {NewA, NewB}.

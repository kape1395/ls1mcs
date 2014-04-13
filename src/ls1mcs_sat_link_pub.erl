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
%%  Event manager for sent and received LS1P frames.
%%
-module(ls1mcs_sat_link_pub).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0, add_handler/2, sent/1, recv/1]).
-include("ls1p.hrl").
-include("ls1mcs.hrl").


%% =============================================================================
%%  API Function Definitions
%% =============================================================================

%%
%%  Start the link.
%%
-spec start_link() -> {ok, pid()}.

start_link() ->
    gen_event:start_link({local, ?MODULE}).


%%
%%  Register an event handler that will get all the sent and received frames.
%%
-spec add_handler(module() | {module(), term()}, term()) -> ok.

add_handler(Handler, Args) ->
    ok = gen_event:add_sup_handler(?MODULE, Handler, Args).


%%
%%  Notify handlers about sent frame.
%%
sent(Ls1pFrame) ->
    ok = gen_event:notify(?MODULE, {sent, Ls1pFrame}).

%%
%%  Notify handlers about received frame.
%%
recv(Ls1pFrame) ->
    ok = gen_event:notify(?MODULE, {recv, Ls1pFrame}).



%% =============================================================================
%%  Internal Functions.
%% =============================================================================



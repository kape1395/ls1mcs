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
%%  Entry point to SAT Link.
%%
-module(ls1mcs_sat_link).
-compile([{parse_transform, lager_transform}]).
-export([send/1, recv/1, add_handler/2, reg_sender/0]).
-include("ls1p.hrl").
-include("ls1mcs.hrl").

-define(LINK_TNC_NAME, ls1mcs_sat_link_tnc).


%% =============================================================================
%%  API Function Definitions
%% =============================================================================

%%
%%  Send command to the SAT.
%%  NOTE: The frames are not logged to the store here, because we have no
%%  bytes here. The logging is therefore implemented in the `ls1mcs_proto_ls1p`.
%%
-spec send(#ls1p_cmd_frame{}) -> ok.

send(Ls1pCmdFrame) when is_record(Ls1pCmdFrame, ls1p_cmd_frame) ->
    ok = ls1mcs_tnc:send(?LINK_TNC_NAME, Ls1pCmdFrame),
    ok = ls1mcs_sat_link_pub:sent(Ls1pCmdFrame).


%%
%%  Receives incoming messages from the TNC.
%%  NOTE: The frames are not logged to the store here, because we have no
%%  bytes here. The logging is therefore implemented in the `ls1mcs_proto_ls1p`.
%%
-spec recv(#ls1p_ack_frame{} | #ls1p_data_frame{} | #ls1p_tm_frame{}) -> ok.

recv(Ls1pFrame) ->
    ok = ls1mcs_sat_link_pub:recv(Ls1pFrame).


%%
%%  Register an event handler that will get all the sent and received frames.
%%
-spec add_handler(module() | {module(), term()}, term()) -> ok.

add_handler(Handler, Args) ->
    ok = ls1mcs_sat_link_pub:add_handler(Handler, Args).


%%
%%  Register a TNC to this SAT Link.
%%
-spec reg_sender() -> ok.

reg_sender() ->
    true = register(?LINK_TNC_NAME, self()),
    ok.



%% =============================================================================
%%  Internal Functions.
%% =============================================================================



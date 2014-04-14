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
-export([send/1, recv/1, add_send_handler/2, add_recv_handler/2]).
-include("ls1p.hrl").
-include("ls1mcs.hrl").


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
    ok = ls1mcs_sat_link_hub:send(Ls1pCmdFrame),
    ok = ls1mcs_sat_link_pub:sent(Ls1pCmdFrame).


%%
%%  Receives incoming messages from the TNC.
%%  NOTE: The frames are not logged to the store here, because we have no
%%  bytes here. The logging is therefore implemented in the `ls1mcs_proto_ls1p`.
%%
-spec recv(list() | #ls1p_ack_frame{} | #ls1p_data_frame{} | #ls1p_tm_frame{}) -> ok.

recv(Ls1pFrames) when is_list(Ls1pFrames) ->
    [ ok = recv(F) || F <- Ls1pFrames ],
    ok;

recv(Ls1pFrame) ->
    ok = ls1mcs_sat_link_pub:recv(Ls1pFrame).


%%
%%  Register an event handler that will get all the sent and received frames.
%%
-spec add_send_handler(module() | {module(), term()}, term()) -> ok.

add_send_handler(Handler, Args) ->
    ok = ls1mcs_sat_link_hub:add_handler(Handler, Args).


%%
%%  Register an event handler that will get all the sent and received frames.
%%
-spec add_recv_handler(module() | {module(), term()}, term()) -> ok.

add_recv_handler(Handler, Args) ->
    ok = ls1mcs_sat_link_pub:add_handler(Handler, Args).



%% =============================================================================
%%  Internal Functions.
%% =============================================================================



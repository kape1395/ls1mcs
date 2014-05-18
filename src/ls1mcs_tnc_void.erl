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
%%  Void TNC, does nothing apart from logging.
%%
-module(ls1mcs_tnc_void).
-behaviour(ls1mcs_tnc).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/2, send/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("ls1mcs.hrl").


%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%
%%
start_link(Name, Password) ->
    {ok, Pid} = gen_server:start_link({via, gproc, Name}, ?MODULE, {Password}, []),
    ok = ls1mcs_tnc:register(?MODULE, Name),
    {ok, Pid}.


%%
%%
%%
send(Ref, Frame) ->
    gen_server:call({via, gproc, Ref}, {send, Frame}).



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    send,
    recv
}).


%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================


%%
%%
%%
init({Password}) ->
    {ok, Ls1pSend} = ls1mcs_proto_ls1p:make_ref(Password, true),
    {ok, Ls1pRecv} = ls1mcs_proto_ls1p:make_ref(Password, true),
    {ok, Send} = ls1mcs_proto:make_send_chain([Ls1pSend]),
    {ok, Recv} = ls1mcs_proto:make_recv_chain([Ls1pRecv]),
    State = #state{
        send = Send,
        recv = Recv
    },
    {ok, State}.


%%
%%
%%
handle_call({send, Frame}, _From, State = #state{send = SendChain}) ->
    {ok, BinFrames, NewSendChain} = ls1mcs_proto:send(Frame, SendChain),
    lager:info("Got request to send frame: ~p as ~p", [Frame, BinFrames]),
    {reply, ok, State#state{send = NewSendChain}}.


%%
%%
%%
handle_cast(_Message, State) ->
    {stop, not_implemented, State}.


%%
%%
%%
handle_info(_Message, State) ->
    {stop, not_implemented, State}.


%%
%%  Terminate a process.
%%
terminate(_Reason, _State) ->
    ok.


%%
%%
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% =============================================================================
%%  Internal Functions.
%% =============================================================================


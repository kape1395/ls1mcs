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
%%  Supervises SAT Link related processes.
%%
-module(ls1mcs_sat_link_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

-define(LS1P_MOD,  ls1mcs_proto_ls1p).
-define(LS1P_NAME, {n, l, ?LS1P_MOD}).


%% =============================================================================
%%  API functions.
%% =============================================================================

%%
%%  Create this supervisor.
%%
start_link(LinkOpts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {LinkOpts}).



%% =============================================================================
%%  Callbacks for supervisor.
%% =============================================================================

%%
%% Supervisor initialization (CB).
%%
init({LinkOpts}) ->
    Ls1pPassword = proplists:get_value(ls1p_password, LinkOpts),
    ThisCallSign = proplists:get_value(this_callsign, LinkOpts),
    PeerCallSign = proplists:get_value(peer_callsign, LinkOpts),
    TncName = {n, l, ls1mcs_sat_link_tnc},
    {TncMod, TncArgs} = case proplists:get_value(tnc, LinkOpts) of
        {wa8ded_hostmode, TncOpts} ->
            Device = proplists:get_value(device, TncOpts),
            {ls1mcs_tnc_wa8ded_hm, [TncName, Device, Ls1pPassword, ThisCallSign]};
        {tapr_kiss, TncOpts} ->
            Device = proplists:get_value(device, TncOpts),
            {ls1mcs_tnc_tapr_kiss, [TncName, Device, Ls1pPassword, ThisCallSign, PeerCallSign]};
        {mfj1270c_kiss, TncOpts} ->
            Device = proplists:get_value(device, TncOpts),
            {ls1mcs_tnc_mfj1270c, [TncName, Device, Ls1pPassword, ThisCallSign, PeerCallSign]};
        {soundmodem_rs232, TncOpts} ->
            Device = proplists:get_value(device, TncOpts),
            {ls1mcs_tnc_smodem, [TncName, Device, Ls1pPassword, ThisCallSign, PeerCallSign]};
        {soundmodem_agwpe, TncOpts} ->
            ConnHost  = proplists:get_value(conn_host,  TncOpts),
            ConnPort  = proplists:get_value(conn_port,  TncOpts),
            AgwpeUser = proplists:get_value(agwpe_user, TncOpts),
            AgwpePass = proplists:get_value(agwpe_pass, TncOpts),
            AgwpePort = proplists:get_value(agwpe_port, TncOpts),
            AgwpeOpts = #{
                port => AgwpePort,
                call => ThisCallSign,
                peer => PeerCallSign,
                user => AgwpeUser,
                pass => AgwpePass
            },
            {ls1mcs_tnc_agwpe, [TncName, ConnHost, ConnPort, Ls1pPassword, AgwpeOpts]};
        {file, TncOpts} ->
            DataDir = proplists:get_value(data_dir, TncOpts),
            {ls1mcs_tnc_file, [TncName, Ls1pPassword, DataDir]};
        {void, _TncOpts} ->
            {ls1mcs_tnc_void, [TncName]}
    end,
    {ok, {{one_for_all, 100, 10}, [
        {pub,  {ls1mcs_sat_link_pub, start_link, []}, permanent, 5000, worker, [ls1mcs_sat_link_pub]},
        {hub,  {ls1mcs_sat_link_hub, start_link, []}, permanent, 5000, worker, [ls1mcs_sat_link_hub]},
        {tnc,  {TncMod, start_link, TncArgs},         permanent, 5000, worker, [TncMod]}
    ]}}.



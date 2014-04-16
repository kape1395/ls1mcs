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
%%  TNC that writes commands to files in the specified folder.
%%  Files contain commands with He-100 and AX25 headers, as they are
%%  being sent from He-100 to ARM.
%%
%%  Used for testing as illustrated here:
%%  http://devopsreactions.tumblr.com/post/61394221619/systems-engineering-without-devops-tools
%%
-module(ls1mcs_tnc_file).
-behaviour(ls1mcs_tnc).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/3, send/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("ls1mcs.hrl").


%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%
%%
start_link(Name, Password, DataDir) ->
    {ok, Pid} = gen_server:start_link({via, gproc, Name}, ?MODULE, {Password, DataDir}, []),
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
    data_dir,
    file_idx,
    send,
    recv
}).


%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================


%%
%%
%%
init({Password, DataDir}) ->
    {ok, Ls1pSend} = ls1mcs_proto_ls1p:make_ref(Password, true),
    {ok, Ls1pRecv} = ls1mcs_proto_ls1p:make_ref(Password, true),
    {ok, Send} = ls1mcs_proto:make_send_chain([Ls1pSend]),
    {ok, Recv} = ls1mcs_proto:make_recv_chain([Ls1pRecv]),
    setup_scan_timer(),
    State = #state{
        data_dir = DataDir,
        file_idx = 0,
        send = Send,
        recv = Recv
    },
    {ok, State}.


%%
%%
%%
handle_call({send, Frame}, _From, State = #state{data_dir = DataDir, file_idx = FileIndex, send = SendChain}) ->
    SendFrameFun = fun (BinFrame, Index) ->
        send_to_file(BinFrame, DataDir, Index),
        Index + 1
    end,
    {ok, BinFrames, NewSendChain} = ls1mcs_proto:send(Frame, SendChain),
    NewFileIndex = lists:foldl(SendFrameFun, FileIndex, BinFrames),
    {reply, ok, State#state{file_idx = NewFileIndex, send = NewSendChain}}.


%%
%%
%%
handle_cast(_Message, State) ->
    {stop, not_implemented, State}.


%%
%%
%%
handle_info({resp_scan_timer}, State = #state{data_dir = DataDir, recv = RecvChain}) ->
    {ok, Filenames} = file:list_dir(DataDir),
    {ok, Pattern} = re:compile(".*_reply$"),
    ProcessFun = fun (Filename, RC) ->
        case re:run(Filename, Pattern) of
            nomatch ->
                ok;
            {match, _} ->
                FilePath = lists:flatten([DataDir, "/", Filename]),
                {ok, FramesBin} = recv_from_file(FilePath),
                {ok, Frames, NewRC} = ls1mcs_proto:recv(FramesBin, RC),
                ok = ls1mcs_sat_link:recv(Frames),
                ok = file:rename(FilePath, [FilePath, ".processed"]),
                NewRC
        end
    end,
    NewRecvChain = lists:foldl(ProcessFun, RecvChain, Filenames),
    setup_scan_timer(),
    {noreply, State#state{recv = NewRecvChain}}.


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

%%
%%  Send to file with He and AX25 headers.
%%
send_to_file(Data, DataDir, FileIndex) ->
    HePayload = <<
        0:128,              %%  AX25 HDR: Addr + Addt + Ctrl + PID = 16 bytes
        Data/binary,        %%  AX25 Info: LS1P command
        0:16                %%  AX25 FCK = 2 bytes, not used.
    >>,
    HePayloadLen = size(HePayload),
    %%
    HeHdrFields = <<
        16#2004:16,
        HePayloadLen:16
    >>,
    HeHdrCkSum = checksum(HeHdrFields),
    %%
    HeFrameWoMarker = <<
        HeHdrFields/binary,
        HeHdrCkSum/binary,
        HePayload/binary
    >>,
    HePayloadCksum = checksum(HeFrameWoMarker),
    HeFrame = <<
        "He",
        HeFrameWoMarker/binary,
        HePayloadCksum/binary
    >>,
    %%
    Filename = lists:flatten(io_lib:format("~s/ls1p_cmd-~8..0w", [DataDir, FileIndex])),
    file:write_file(Filename, HeFrame),
    lager:info("Command written to file ~p, ls1p bytes: ~p.", [Filename, Data]).


%%
%%  Recv from file with He headers.
%%
recv_from_file(Filename) ->
    lager:info("Reading response from file ~p", [Filename]),
    {ok, HeFrame} = file:read_file(Filename),
    <<"He", HeHdrFields:4/binary, HeHdrCkSum:2/binary, HePayloadWithCkSum/binary>> = HeFrame,
    <<HeCmd:16, HeLen:16>> = HeHdrFields,
    HeCmd = 16#1003,
    HeHdrCkSum = checksum(HeHdrFields),
    <<HePayload:HeLen/binary, HePayloadCksum/binary>> = HePayloadWithCkSum,
    HePayloadCksum = checksum(<<HeHdrFields/binary, HeHdrCkSum/binary, HePayload/binary>>),
    {ok, HePayload}.



%%
%%  Two checksum bytes are appended to the header for
%%  error detection. The 8-bit Fletcher algorithm (see
%%  RFC 1145 which describes TCP) is used to calculate
%%  the checksums.
%%
checksum(Data) ->
    {ok, Cksum} = ls1mcs_utl_cksum:checksum(Data, fletcher8bit),
    Cksum.


%%
%%
%%
setup_scan_timer() ->
    erlang:send_after(10000, self(), {resp_scan_timer}).



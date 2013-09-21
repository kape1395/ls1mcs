%%
%%  TNC that writes commands to files in the specified folder.
%%  Files contain commands with He-100 and AX25 headers, as they are
%%  being sent from He-100 to ARM.
%%
%%  Used for testing as illustrated here:
%%  http://devopsreactions.tumblr.com/post/61394221619/systems-engineering-without-devops-tools
%%
-module(ls1mcs_tnc_file).
-behaviour(ls1mcs_protocol).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/3, send/2, received/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("ls1mcs.hrl").


%% =============================================================================
%%  Public API
%% =============================================================================


%%
%%
%%
start_link(Name, Upper, DataDir) ->
    gen_server:start_link({via, gproc, Name}, ?MODULE, {Upper, DataDir}, []).


%%
%%
%%
send(Ref, Data) ->
    gen_server:call({via, gproc, Ref}, {send, Data}).


%%
%%  Not used here.
%%
received(_Ref, _Data) ->
    ok.



%% =============================================================================
%%  Internal data structures.
%% =============================================================================

-record(state, {
    upper,
    data_dir,
    file_idx
}).


%% =============================================================================
%%  Callbacks for gen_server.
%% =============================================================================


%%
%%
%%
init({Upper, DataDir}) ->
    self() ! {initialize},
    State = #state{
        upper = Upper,
        data_dir = DataDir,
        file_idx = 0
    },
    {ok, State}.


%%
%%
%%
handle_call({send, Data}, _From, State = #state{data_dir = DataDir, file_idx = FileIndex}) ->
    send_to_file(Data, DataDir, FileIndex),
    {reply, ok, State#state{file_idx = FileIndex + 1}}.


%%
%%
%%
handle_cast(_Message, State) ->
    {stop, not_implemented, State}.


%%
%%
%%
handle_info({initialize}, State = #state{upper = Upper}) ->
    ls1mcs_protocol:await(Upper),
    lager:debug("Upper protocol started"),
    setup_scan_timer(),
    {noreply, State};

handle_info({resp_scan_timer}, State = #state{data_dir = DataDir, upper = Upper}) ->
    {ok, Filenames} = file:list_dir(DataDir),
    {ok, Pattern} = re:compile(".*_reply$"),
    ProcessFun = fun (Filename) ->
        case re:run(Filename, Pattern) of
            nomatch ->
                ok;
            {match, _} ->
                FilePath = lists:flatten([DataDir, "/", Filename]),
                ok = recv_from_file(FilePath, Upper),
                ok = file:rename(FilePath, [FilePath, ".processed"])
        end
    end,
    lists:foreach(ProcessFun, Filenames),
    setup_scan_timer(),
    {noreply, State}.


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
recv_from_file(Filename, Upper) ->
    lager:info("Reading response from file ~p", [Filename]),
    {ok, HeFrame} = file:read_file(Filename),
    <<"He", HeCmd:16, HeLen:16, HeHdrCkSum:16, HePayloadWithCkSum/binary>> = HeFrame,
    HeCmd = 16#1003,
    HeHdrCkSum = checksum(<<HeCmd:16, HeLen:16>>),
    <<HePayload:HeLen/binary, HePayloadCksum>> = HePayloadWithCkSum,
    HePayloadCksum = checksum(<<HeCmd:16, HeLen:16, HeHdrCkSum:16, HePayload:HeLen/binary>>),
    ok = ls1mcs_protocol:received(Upper, HePayload).



%%
%%  Two checksum bytes are appended to the header for
%%  error detection. The 8-bit Fletcher algorithm (see
%%  RFC 1145 which describes TCP) is used to calculate
%%  the checksums.
%%
checksum(Data) ->
    {A, B} = lists:foldl(fun checksum_fold/2, {0, 0}, erlang:binary_to_list(Data)),
    <<A:8, B:8>>.

checksum_fold(Byte, {A, B}) ->
    NewA = 16#FF band (A + Byte),
    NewB = 16#FF band (B + NewA),
    {NewA, NewB}.


%%
%%
%%
setup_scan_timer() ->
    erlang:send_after(10000, self(), {resp_scan_timer}).



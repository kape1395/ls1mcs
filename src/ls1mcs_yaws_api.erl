%%
%%  Handling of REST style requests.
%%  http://stateless.co/hal_specification.html
%%
-module(ls1mcs_yaws_api).
-compile([{parse_transform, lager_transform}]).
-export([handle_request/3]).
-include("ls1mcs.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-define(MEDIATYPE_JSON, "application/vnd.ls1mcs-v1+json; level=0").
-define(MEDIATYPE_TERM, "application/x-erlang-term").
-define(MEDIATYPE_JPEG, "image/jpeg").
-define(MEDIATYPE_TXT,  "text/plain; charset=utf8").
-define(MEDIATYPE_BIN,  "application/octet-stream").


%% =============================================================================
%%  Resource handling functions.
%% =============================================================================

%% -----------------------------------------------------------------------------
%%  Root.
%% -----------------------------------------------------------------------------

handle_request([], 'GET', _Arg) ->
    respond(200, json_object({root}));


%% -----------------------------------------------------------------------------
%%  Commands
%% -----------------------------------------------------------------------------

%%
%%  Root resource for commands.
%%
handle_request(["command"], 'GET', _Arg) ->
    respond(200, json_object({command}));

%%
%%  Command addresses.
%%
handle_request(["command", "group"], 'GET', _Arg) ->
    CmdGroups = ls1mcs_usr_cmd:groups(),
    respond(200, json_list(CmdGroups));

%%
%%  User command specs.
%%
handle_request(["command", "spec"], 'GET', _Arg) ->
    UserCmdSpecs = ls1mcs_usr_cmd:specs(),
    respond(200, json_list(UserCmdSpecs));

handle_request(["command", "spec", Group], 'GET', _Arg) ->
    GroupAtom = ls1mcs_yaws_json:decode_atom(Group),
    UserCmdSpecs = lists:filter(
        fun (#usr_cmd_spec{group = G}) -> GroupAtom =:= G end,
        ls1mcs_usr_cmd:specs()
    ),
    respond(200, json_list(UserCmdSpecs));

%%
%%  User commands (RO).
%%
handle_request(["command", "usr"], 'GET', Arg) ->
    {ok, UsrCmds} = ls1mcs_store:get_usr_cmds(all),
    case yaws_api:queryvar(Arg, "t") of
        {ok, "txt"} ->
            Headers = [{header, {"Content-Disposition", "attachment; filename=\"ls1mcs_usr_cmd_log.txt\""}}],
            respond(200, ?MEDIATYPE_TXT, Headers, erlang:iolist_to_binary(ls1mcs_yaws_txt:encode_list(UsrCmds)));
        _ ->
            respond(200, json_list(UsrCmds))
    end;

handle_request(["command", "usr", Id], 'GET', _Arg) ->
    case ls1mcs_store:get_usr_cmds({id, ls1mcs_yaws_json:decode_integer(Id)}) of
        {ok, [Command]} -> respond(200, json_object(Command));
        {ok, []} -> respond_error(404, <<"Command not found by id.">>)
    end;

handle_request(["command", "usr", Id, "photo"], 'GET', _Arg) ->
    case ls1mcs_store:get_usr_cmd(ls1mcs_yaws_json:decode_integer(Id), all) of
        {ok, #usr_cmd{spec = Spec}, SatCmds} when Spec =:= photo_data; Spec =:= dlnk_photo ->
            Frames = [ {CmdFrame, DataFrames} ||
                {
                    _SatCmd,
                    CmdFrame = #ls1p_cmd_frame{addr = arduino, port = photo_data},
                    _AckFrame,
                    DataFrames
                } <- SatCmds
            ],
            {ok, PhotoContent} = ls1mcs_proto_ls1p:merged_response(Frames),
            respond(200, ?MEDIATYPE_JPEG, PhotoContent);
        {error, not_found} ->
            respond_error(404, <<"Command not found by id.">>)
    end;

handle_request(["command", "usr", Id], 'PUT', Arg) ->
    Json = jiffy:decode(Arg#arg.clidata),
    case Json of
        {[{<<"status">>, <<"confirmed">>}]} ->
            UsrCmdId = ls1mcs_yaws_json:decode_integer(Id),
            lager:notice("Confirming user command id=~p", [Id]),
            ls1mcs_store:set_usr_cmd_status(UsrCmdId, confirmed),
            respond(200, <<"">>);
        _ ->
            lager:notice("Unrecognized update action ~p for user command id=~p", [Json, Id]),
            respond_error(400, <<"Unrecognized update action.">>)
    end;

%%
%%  SAT commands (RO).
%%
handle_request(["command", "sat"], 'GET', _Arg) ->
    {ok, SatCmds} = ls1mcs_store:get_sat_cmds(all),
    respond(200, json_list(SatCmds));

handle_request(["command", "sat", Id], 'GET', _Arg) ->
    case ls1mcs_store:get_sat_cmds({id, ls1mcs_yaws_json:decode_integer(Id)}) of
        {ok, [Command]} -> respond(200, json_object(Command));
        {ok, []} -> respond_error(404, <<"Command not found by id.">>)
    end;

%%
%%  Immediate command (RW).
%%
handle_request(["command", "immediate"], 'GET', _Arg) ->
    respond(200, json_list([]));    % TODO

handle_request(["command", "immediate"], 'POST', Arg) ->
    UserCmd = ls1mcs_yaws_json:decode(usr_cmd, jiffy:decode(Arg#arg.clidata)),
    Now = erlang:now(),
    {ok, UserCmdId} = ls1mcs_usr_cmd:issue(UserCmd#usr_cmd{
        id = undefined,
        immediate = true,
        approved = Now,
        issued = Now
    }),
    respond(200, json_self(usr_cmd, UserCmdId));

handle_request(["command", "immediate", _CommandId], 'GET', _Arg) ->
    respond(200, json_list([]));    % TODO

handle_request(["command", "plan", _CommandPlanId], 'GET', _Arg) ->
    respond(200, json_list([]));    % TODO

handle_request(["command", "plan", _CommandPlanId, "commmand"], 'GET', _Arg) ->
    respond(200, json_list([]));    % TODO

%% -----------------------------------------------------------------------------
%%  LS1P Frames
%% -----------------------------------------------------------------------------

handle_request(["ls1p_frame"], 'GET', _Arg) ->
    {ok, Frame} = ls1mcs_store:get_ls1p_frame({cmd, all}),
    respond(200, ls1mcs_yaws_json:encode_list(Frame));

handle_request(["ls1p_frame", FrameId], 'GET', _Arg) ->
    CRef = ls1mcs_yaws_json:decode(cref, FrameId),
    case ls1mcs_store:get_ls1p_frame({cmd, CRef}) of
        {ok, Frame} -> respond(200, ls1mcs_yaws_json:encode(Frame));
        {error, not_found} -> respond_error(404, <<"Not found.">>)
    end;

handle_request(["ls1p_frame", FrameId, "ack"], 'GET', _Arg) ->
    CRef = ls1mcs_yaws_json:decode(cref, FrameId),
    {ok, Frames} = ls1mcs_store:get_ls1p_frame({ack, CRef}),
    respond(200, ls1mcs_yaws_json:encode_list(Frames));

handle_request(["ls1p_frame", FrameId, "data"], 'GET', _Arg) ->
    CRef = ls1mcs_yaws_json:decode(cref, FrameId),
    {ok, Frames} = ls1mcs_store:get_ls1p_frame({data, CRef}),
    respond(200, ls1mcs_yaws_json:encode_list(Frames));

handle_request(["ls1p_frame", FrameId, "data", "content"], 'GET', _Arg) ->
    CRef = ls1mcs_yaws_json:decode(cref, FrameId),
    {ok, CmdFrame} = ls1mcs_store:get_ls1p_frame({cmd, CRef}),
    {ok, DataFrames} = ls1mcs_store:get_ls1p_frame({data, CRef}),
    {ok, MergedData} = ls1mcs_proto_ls1p:merged_response(CmdFrame, DataFrames),
    respond(200, media_type_for_response(CmdFrame), MergedData);

handle_request(["ls1p_frame", FrameId, "recv"], 'GET', _Arg) ->
    CRef = ls1mcs_yaws_json:decode(cref, FrameId),
    {ok, Frames} = ls1mcs_store:get_ls1p_frame({recv, CRef}),
    respond(200, ls1mcs_yaws_json:encode_list(Frames));


%% -----------------------------------------------------------------------------
%%  Telemetry
%% -----------------------------------------------------------------------------

handle_request(["telemetry"], 'GET', _Arg) ->
    respond(200, json_object({telemetry}));

%%
%%  Telemetry collected by ground station.
%%  Examples:
%%
%%      http://localhost:8000/ls1mcs/api/telemetry/gs
%%      http://localhost:8000/ls1mcs/api/telemetry/gs?t=txt
%%      http://localhost:8000/ls1mcs/api/telemetry/gs?from=2014-05-10&till=2014-05-13
%%
handle_request(["telemetry", "gs"], 'GET', Arg) ->
    Query = case {yaws_api:queryvar(Arg, "from"), yaws_api:queryvar(Arg, "till")} of
        {undefined,  undefined } -> all;
        {{ok, From}, {ok, Till}} -> {time, parse_tstamp(From), parse_tstamp(Till)};
        {undefined,  {ok, Till}} -> {time, undefined,          parse_tstamp(Till)};
        {{ok, From}, undefined } -> {time, parse_tstamp(From), undefined}
    end,
    {ok, TMFrames} = ls1mcs_store:get_tm(Query),
    case yaws_api:queryvar(Arg, "t") of
        {ok, "txt"} ->
            respond(200, ?MEDIATYPE_TXT, erlang:iolist_to_binary(ls1mcs_yaws_txt:encode_list(TMFrames)));
        _ ->
            respond(200, json_list(TMFrames))
    end;

handle_request(["telemetry", "gs", "latest"], 'GET', _Arg) ->
    case ls1mcs_store:get_tm(latest) of
        {ok, [TMFrame]} -> respond(200, json_object(TMFrame));
        {ok, []} -> respond_error(404, <<"Have no telemetry yet.">>)
    end;

handle_request(["telemetry", "gs", "5646"], 'GET', _Arg) ->
    % TODO
    [
        {status, 200}%,
        %{content, ?MEDIATYPE_JSON, jiffy:encode({[
        %    {'_links', {[
        %        {self, {[{self, url(["telemetry", "5646"])}]}}
        %    ]}},
        %    {field1, 3},
        %    {field2, 3.1},
        %    {field3, 3.14},
        %    {field4, 3.142},
        %    {field5, 3.1415}
        %]})}
    ];

%%
%%  HAM Telemetry
%%
handle_request(["telemetry", "ham"], 'GET', _Arg) ->
    respond(200, json_list([]));    % TODO

%%
%%  curl --request POST --data-binary "@14042014_1658.bin" "http://localhost:8000/ls1mcs/api/telemetry/ham/?t=json&m=preview"
%%
handle_request(["telemetry", "ham"], 'POST', Arg = #arg{headers = Headers}) ->
    {ok, Ls1pRecv} = ls1mcs_proto_ls1p:make_ref(<<0:16>>, false),
    {ok, Ax25Recv} = ls1mcs_proto_ax25:make_ref("NOCALL", "NOCALL", tnc),
    {ok, KissRecv} = ls1mcs_proto_kiss:make_ref(),
    {ok, RecvChain} = ls1mcs_proto:make_recv_chain([KissRecv, Ax25Recv, Ls1pRecv]),
    FileHandler = fun (RawBytes) ->
        {ok, RecvFrames, _NewRecvChain} = ls1mcs_proto:recv(RawBytes, RecvChain),
        TmFrames = [ F || F = #ls1p_tm_frame{} <- RecvFrames ],
        MediaType = case yaws_api:queryvar(Arg, "t") of
            {ok, "json"} -> json;
            _ -> html
        end,
        case MediaType of
            json ->
                respond(200, json_list(TmFrames));
            html ->
                {ehtml, [
                    {body, [], [
                        {pre, [], [{code, [{id, "tm-data"}], jiffy:encode(json_list(TmFrames))}]},
                        {script, [{src, "/ls1mcs/gui/tmp/js/vendor/jquery-1.8.3.min.js"}], []},
                        {script, [{type, "text/javascript"}], <<"$('#tm-data').html(JSON.stringify(JSON.parse($('#tm-data').html()), undefined, 4));">>}
                    ]}
                ]}
        end
    end,
    case lists:prefix("multipart/form-data", yaws_api:get_header(Headers, content_type)) of
        true  -> process_multipart_single_file(FileHandler, Arg);
        false -> process_entity(FileHandler, Arg)
    end;


%%
%%  Telemetry archive.
%%
handle_request(["telemetry", "archive"], 'GET', _Arg) ->
    respond(200, json_list([]));    % TODO


%% -----------------------------------------------------------------------------
%%  SAT Position
%% -----------------------------------------------------------------------------

%%
%%  SAT position, example call:
%%      $ curl http://localhost:12321/ls1mcs/api/sat/LS1/position/predicted/current;
%%
handle_request(["sat", _SAT, "position", "predicted", "current"], 'GET', _Arg) ->
    Timestamp = {A, B, C} = erlang:now(),
    X = (A * 1000000 + B + C / 1000000) / 60,
    Precision = 1000,
    Longitude = (round(X * 180 * Precision) rem (360 * Precision)) / Precision - 180,
    Latitude = math:sin(X) * 90,
    Altitude = 400000,
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[
            {'_links', {[
            ]}},
            {timestamp, ls1mcs_yaws_json:encode_tstamp(Timestamp)},
            {longitude, Longitude},
            {latitude, Latitude},
            {altitude, Altitude}
        ]})}
    ].

%%
%%  Other resources
%%



%% =============================================================================
%%  Helper functions.
%% =============================================================================

%%
%%
%%
json_object(Object) ->
    ls1mcs_yaws_json:encode(Object).


%%
%%
%%
json_list(Objects) ->
    ls1mcs_yaws_json:encode_list(Objects).


%%
%%
%%
json_self(Type, Id) ->
    ls1mcs_yaws_json:encode_self(Type, Id).


%%
%%
%%
respond(Status, Response) ->
    [
        {status, Status},
        {content, ?MEDIATYPE_JSON, jiffy:encode(Response)}
    ].


%%
%%
%%
respond(Status, MediaType, Response) ->
    [
        {status, Status},
        {content, MediaType, Response}
    ].


%%
%%
%%
respond(Status, MediaType, Other, Response) ->
    Other ++ [
        {status, Status},
        {content, MediaType, Response}
    ].


%%
%%
%%
respond_error(Status, ReasonMsg) ->
    [
        {status, Status},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[
            {code, unknown},
            {msg, ReasonMsg}
        ]})}
    ].


%%
%%  Determine media type for response data of the specified command.
%%
media_type_for_response(CmdFrame) ->
    case CmdFrame of
        #ls1p_cmd_frame{addr = arduino, port = photo_data} ->
            ?MEDIATYPE_JPEG;
        _ ->
            ?MEDIATYPE_BIN
    end.


%%
%%  Single file upload (multipart).
%%
process_multipart_single_file(HandlerFun, Arg = #arg{state = ArgState}) ->
    State = case ArgState of
        undefined -> [];
        _ -> ArgState
    end,
    ProcessPartFun = fun
        ({head, {_Name, _Hdrs}}, [])    -> [];
        ({part_body, Part},      Parts) -> [Part | Parts];
        ({body,      Part},      Parts) -> [Part | Parts]
    end,
    case yaws_api:parse_multipart_post(Arg) of
        {cont, Cont, Res} ->
            NewState = lists:foldl(ProcessPartFun, State, Res),
            {get_more, Cont, NewState};
        {result, Res} ->
            NewState = lists:foldl(ProcessPartFun, State, Res),
            FileContents = erlang:iolist_to_binary(lists:reverse(NewState)),
            HandlerFun(FileContents);
        {error, Reason} ->
            lager:error("Unable to parse multipart POST, error=~p", [Reason]),
            respond_error(12321, <<"Failed to parse multipart POST.">>)
    end.

%%
%%  Single file upload (multipart).
%%
process_entity(HandlerFun, #arg{state = ArgState, clidata = CliData}) ->
    State = case ArgState of
        undefined -> [];
        _ -> ArgState
    end,
    case CliData of
        {partial, Data} ->
            {get_more, undefined, [Data | State]};
        Data when is_binary(Data) ->
            NewState = [Data | State],
            FileContents = erlang:iolist_to_binary(lists:reverse(NewState)),
            HandlerFun(FileContents);
        {error, Reason} ->
            lager:error("Unable to parse POST, error=~p", [Reason]),
            respond_error(12121, <<"Failed to parse POST.">>)
    end.


%%
%%  Decode timestamp.
%%  calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).
%%  Eg. "2013/07/21 23:57:26".
%%
-define(UNIX_BIRTH, 62167219200).
-define(MEGA_SECS, 1000000).
parse_tstamp(TStamp) when is_list(TStamp) ->
    parse_tstamp(erlang:list_to_binary(TStamp));

parse_tstamp(undefined) ->
    undefined;

parse_tstamp(DateBin) ->
    case DateBin of
        <<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary>> ->
            Hour = <<"00">>, Min = <<"00">>, Sec = <<"00">>, ok;
        <<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary, "T", Hour:2/binary, ":", Min:2/binary, ":", Sec:2/binary>> ->
            ok
    end,
    Date = {
        {binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)},
        {binary_to_integer(Hour), binary_to_integer(Min), binary_to_integer(Sec)}
    },
    DateSecs = calendar:datetime_to_gregorian_seconds(Date) - ?UNIX_BIRTH,
    {DateSecs div ?MEGA_SECS, DateSecs rem ?MEGA_SECS, 0}.



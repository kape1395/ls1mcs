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
%%  Command address.
%%
handle_request(["command", "address"], 'GET', _Arg) ->
    CommandAddrs = ls1mcs_command:command_addresses(),
    respond(200, json_list(CommandAddrs));

%%
%%  User command spec.
%%
handle_request(["command", "specification"], 'GET', _Arg) ->
    UserCmdSpecs = ls1mcs_command:user_cmd_specs(),
    respond(200, json_list(UserCmdSpecs));

handle_request(["command", "specification", Addr], 'GET', _Arg) ->
    AddrAtom = ls1mcs_yaws_json:decode_atom(Addr),
    UserCmdSpecs = lists:filter(
        fun (#user_cmd_spec{addr = A}) -> AddrAtom =:= A end,
        ls1mcs_command:user_cmd_specs()
    ),
    respond(200, json_list(UserCmdSpecs));

%%
%%
%%
handle_request(["___command", Id], 'GET', _Arg) ->
    {ok, Command} = ls1mcs_store:get_command(erlang:list_to_integer(Id)),
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

handle_request(["___command", Id, "response"], 'GET', _Arg) ->
    % TODO
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];


%%
%%  Immediate command.
%%
handle_request(["command", "immediate"], 'GET', _Arg) ->
    % TODO
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

handle_request(["command", "immediate", _CommandId], 'GET', _Arg) ->
    % TODO
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

handle_request(["command", "plan", _CommandPlanId], 'GET', _Arg) ->
    % TODO
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

handle_request(["command", "plan", _CommandPlanId, "commmand"], 'GET', _Arg) ->
    % TODO
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

%%
%%  ls1p_frame
%%

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

handle_request(["ls1p_frame", FrameId, "recv"], 'GET', _Arg) ->
    CRef = ls1mcs_yaws_json:decode(cref, FrameId),
    {ok, Frames} = ls1mcs_store:get_ls1p_frame({recv, CRef}),
    respond(200, ls1mcs_yaws_json:encode_list(Frames));

handle_request(["ls1p_frame", FrameId, "photo"], 'GET', _Arg) ->
    CRef = ls1mcs_yaws_json:decode(cref, FrameId),
    {ok, CmdFrame} = ls1mcs_store:get_ls1p_frame({cmd, CRef}),
    {ok, DataFrames} = ls1mcs_store:get_ls1p_frame({data, CRef}),
    {ok, Photo} = ls1mcs_proto_ls1p:merged_response(CmdFrame, DataFrames),
    [
        {status, 200},
        {content, "image/jpeg", Photo}
    ];

%% -----------------------------------------------------------------------------
%%  Telemetry
%% -----------------------------------------------------------------------------

handle_request(["telemetry"], 'GET', _Arg) ->
    respond(200, json_object({telemetry}));

%%
%%  Telemetry collected by ground station.
%%
handle_request(["telemetry", "gs"], 'GET', _Arg) ->
    {ok, TMFrames} = ls1mcs_store:get_tm(all),
    respond(200, json_list(TMFrames));

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

handle_request(["telemetry", "ham"], 'POST', Arg) ->
    % TODO
    lager:debug("Got file, written to test-telemetry.dat"),
    file:write_file("test-telemetry.dat", Arg#arg.clidata),
    [
        {status, 200}%,
        %{content, ?MEDIATYPE_JSON, jiffy:encode({[
        %    {'_links', {[
        %        {self, {[{self, url(["telemetry", "5646"])}]}}
        %    ]}}
        %]})}
    ];

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
respond(Status, Response) ->
    [
        {status, Status},
        {content, ?MEDIATYPE_JSON, jiffy:encode(Response)}
    ].


respond_error(Status, ReasonMsg) ->
    [
        {status, Status},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[
            {code, unknown},
            {msg, ReasonMsg}
        ]})}
    ].



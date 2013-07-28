%%
%%  Handling of REST style requests.
%%  http://stateless.co/hal_specification.html
%%
-module(ls1mcs_yaws_api).
-compile([{parse_transform, lager_transform}]).
-export([handle_request/3]).
-include("ls1mcs.hrl").
-include("ls1mcs_yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").


handle_request([], 'GET', _Arg) ->
    % TODO
    [
        {status, 200},
        {header, {"Link", "<command>; rel=commands"}},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];


%% -----------------------------------------------------------------------------
%%  Command resources
%% -----------------------------------------------------------------------------

handle_request(["command_address"], 'GET', _Arg) ->
    CommandAddrs = ls1mcs_command:command_addresses(),
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode(ls1mcs_yaws_json:encode_list(CommandAddrs))}
    ];

handle_request(["user_cmd_spec"], 'GET', _Arg) ->
    UserCmdSpecs = ls1mcs_command:user_cmd_specs(),
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode(ls1mcs_yaws_json:encode_list(UserCmdSpecs))}
    ];

handle_request(["user_cmd_spec", Addr], 'GET', _Arg) ->
    AddrAtom = erlang:list_to_existing_atom(Addr),
    UserCmdSpecs = lists:filter(
        fun (#user_cmd_spec{addr = A}) -> AddrAtom =:= A end,
        ls1mcs_command:user_cmd_specs()
    ),
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode(ls1mcs_yaws_json:encode_list(UserCmdSpecs))}
    ];

handle_request(["immediate_command"], 'GET', _Arg) ->
    % TODO
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

handle_request(["immediate_command", _CommandId], 'GET', _Arg) ->
    % TODO
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

handle_request(["command_plan", _CommandPlanId], 'GET', _Arg) ->
    % TODO
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

handle_request(["command_plan", _CommandPlanId, "commmand"], 'GET', _Arg) ->
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
        {error, not_found} -> respond(404, <<"Not found.">>)
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

handle_request(["command", Id], 'GET', _Arg) ->
    {ok, Command} = ls1mcs_store:get_command(erlang:list_to_integer(Id)),
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

handle_request(["command", Id, "response"], 'GET', _Arg) ->
    % TODO
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

%% -----------------------------------------------------------------------------
%%  Telemetry
%% -----------------------------------------------------------------------------

%%
%%  HAM Telemetry
%%
handle_request(["telemetry"], 'POST', Arg) ->
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

handle_request(["telemetry"], 'GET', _Arg) ->
    {ok, TMFrames} = ls1mcs_store:get_tm(all),
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode(ls1mcs_yaws_json:encode_list(TMFrames))}
    ];

handle_request(["telemetry", "latest"], 'GET', _Arg) ->
    {ok, [TMFrame]} = ls1mcs_store:get_tm(latest),   % TODO
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode(ls1mcs_yaws_json:encode(TMFrame))}
    ];

handle_request(["telemetry", "5646"], 'GET', _Arg) ->
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
respond(Status, Response) ->
    ls1mcs_yaws:respond(Status, Response).



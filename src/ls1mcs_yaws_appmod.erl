%%
%%  http://stateless.co/hal_specification.html
%%
-module(ls1mcs_yaws_appmod).
-compile([{parse_transform, lager_transform}]).
-export([out/1]).
-include("ls1mcs.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-define(APP, "ls1mcs").
-define(API, "api").
-define(GUI, "gui").
-define(PUB, "pub").
-define(MCS, "mcs").
-define(HAM, "ham").
-define(MEDIATYPE_JSON, "application/vnd.ls1mcs-v1+json; level=0").
-define(MEDIATYPE_TERM, "application/x-erlang-term").


%% =============================================================================
%%  API functions.
%% =============================================================================

%%
%%
%%
out(Arg) ->
    Uri = yaws_api:request_url(Arg),
    Path = string:tokens(Uri#url.path, "/"),
    Method = yaws_api:http_request_method(Arg#arg.req),
    lager:info("Handling request: path=~p, method=~p", [Path, Method]),
    handle_request(Path, Method, Arg).


%%
%%  Handling of REST style requests.
%%

%% -----------------------------------------------------------------------------
%%  API
%% -----------------------------------------------------------------------------

handle_request([?APP, ?API], 'GET', _Arg) ->
    % TODO
    [
        {status, 200},
        {header, {"Link", "<command>; rel=commands"}},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];


%% -----------------------------------------------------------------------------
%%  Command resources
%% -----------------------------------------------------------------------------

handle_request([?APP, ?API, "command_address"], 'GET', _Arg) ->
    CommandAddrs = ls1mcs_command:command_addresses(),
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode(ls1mcs_yaws_json:encode_list(CommandAddrs))}
    ];

handle_request([?APP, ?API, "user_cmd_spec"], 'GET', _Arg) ->
    UserCmdSpecs = ls1mcs_command:user_cmd_specs(),
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode(ls1mcs_yaws_json:encode_list(UserCmdSpecs))}
    ];

handle_request([?APP, ?API, "user_cmd_spec", Addr], 'GET', _Arg) ->
    AddrAtom = erlang:list_to_existing_atom(Addr),
    UserCmdSpecs = lists:filter(
        fun (#user_cmd_spec{addr = A}) -> AddrAtom =:= A end,
        ls1mcs_command:user_cmd_specs()
    ),
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode(ls1mcs_yaws_json:encode_list(UserCmdSpecs))}
    ];

handle_request([?APP, ?API, "immediate_command"], 'GET', _Arg) ->
    % TODO
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

handle_request([?APP, ?API, "immediate_command", _CommandId], 'GET', _Arg) ->
    % TODO
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

handle_request([?APP, ?API, "command_plan", _CommandPlanId], 'GET', _Arg) ->
    % TODO
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

handle_request([?APP, ?API, "command_plan", _CommandPlanId, "commmand"], 'GET', _Arg) ->
    % TODO
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

%%
%%  ls1p_frame
%%

handle_request([?APP, ?API, "ls1p_frame"], 'GET', _Arg) ->
    {ok, Frame} = ls1mcs_store:get_ls1p_frame({cmd, all}),
    respond(200, ls1mcs_yaws_json:encode_list(Frame));

handle_request([?APP, ?API, "ls1p_frame", FrameId], 'GET', _Arg) ->
    CRef = ls1mcs_yaws_json:decode(cref, FrameId),
    case ls1mcs_store:get_ls1p_frame({cmd, CRef}) of
        {ok, Frame} -> respond(200, ls1mcs_yaws_json:encode(Frame));
        {error, not_found} -> respond(404, <<"Not found.">>)
    end;

handle_request([?APP, ?API, "ls1p_frame", FrameId, "ack"], 'GET', _Arg) ->
    CRef = ls1mcs_yaws_json:decode(cref, FrameId),
    {ok, Frames} = ls1mcs_store:get_ls1p_frame({ack, CRef}),
    respond(200, ls1mcs_yaws_json:encode_list(Frames));

handle_request([?APP, ?API, "ls1p_frame", FrameId, "data"], 'GET', _Arg) ->
    CRef = ls1mcs_yaws_json:decode(cref, FrameId),
    {ok, Frames} = ls1mcs_store:get_ls1p_frame({data, CRef}),
    respond(200, ls1mcs_yaws_json:encode_list(Frames));

handle_request([?APP, ?API, "ls1p_frame", FrameId, "recv"], 'GET', _Arg) ->
    CRef = ls1mcs_yaws_json:decode(cref, FrameId),
    {ok, Frames} = ls1mcs_store:get_ls1p_frame({recv, CRef}),
    respond(200, ls1mcs_yaws_json:encode_list(Frames));

handle_request([?APP, ?API, "ls1p_frame", FrameId, "photo"], 'GET', _Arg) ->
    CRef = ls1mcs_yaws_json:decode(cref, FrameId),
    {ok, CmdFrame} = ls1mcs_store:get_ls1p_frame({cmd, CRef}),
    {ok, DataFrames} = ls1mcs_store:get_ls1p_frame({data, CRef}),
    {ok, Photo} = ls1mcs_proto_ls1p:merged_response(CmdFrame, DataFrames),
    [
        {status, 200},
        {content, "image/jpeg", Photo}
    ];

handle_request([?APP, ?API, "command", Id], 'GET', _Arg) ->
    {ok, Command} = ls1mcs_store:get_command(erlang:list_to_integer(Id)),
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

handle_request([?APP, ?API, "command", Id, "response"], 'GET', _Arg) ->
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
handle_request([?APP, ?API, "telemetry"], 'POST', Arg) ->
    % TODO
    lager:debug("Got file, written to test-telemetry.dat"),
    file:write_file("test-telemetry.dat", Arg#arg.clidata),
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[
            {'_links', {[
                {self, {[{self, list_to_binary(string:join(["", ?APP, ?API, "telemetry", "5646"], "/"))}]}}
            ]}}
        ]})}
    ];

handle_request([?APP, ?API, "telemetry"], 'GET', _Arg) ->
    {ok, TMFrames} = ls1mcs_store:get_tm(all),
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode(ls1mcs_yaws_json:encode_list(TMFrames))}
    ];

handle_request([?APP, ?API, "telemetry", "latest"], 'GET', _Arg) ->
    {ok, [TMFrame]} = ls1mcs_store:get_tm(latest),   % TODO
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode(ls1mcs_yaws_json:encode(TMFrame))}
    ];

handle_request([?APP, ?API, "telemetry", "5646"], 'GET', _Arg) ->
    % TODO
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[
            {'_links', {[
                {self, {[{self, list_to_binary(string:join(["", ?APP, ?API, "telemetry", "5646"], "/"))}]}}
            ]}},
            {field1, 3},
            {field2, 3.1},
            {field3, 3.14},
            {field4, 3.142},
            {field5, 3.1415}
        ]})}
    ];


%% -----------------------------------------------------------------------------
%%  SAT Position
%% -----------------------------------------------------------------------------

%%
%%  SAT position, example call:
%%      $ curl http://localhost:12321/ls1mcs/api/sat/LS1/position/predicted/current;
%%
handle_request([?APP, ?API, "sat", _SAT, "position", "predicted", "current"], 'GET', _Arg) ->
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
            {timestamp, timestamp_to_bin(Timestamp)},
            {longitude, Longitude},
            {latitude, Latitude},
            {altitude, Altitude}
        ]})}
    ];

%%
%%  Other resources
%%




%% -----------------------------------------------------------------------------
%%  GUI
%% -----------------------------------------------------------------------------

handle_request([?APP, ?GUI, ?MCS | Tail] = Path, 'GET', Arg) ->
    serve_index_or_file([?GUI, ?MCS], Tail, Path, Arg);

handle_request([?APP, ?GUI, ?PUB | Tail] = Path, 'GET', Arg) ->
    serve_index_or_file([?GUI, ?PUB], Tail, Path, Arg);

handle_request([?APP, ?GUI, ?HAM | Tail] = Path, 'GET', Arg) ->
    serve_index_or_file([?GUI, ?HAM], Tail, Path, Arg);


%% -----------------------------------------------------------------------------
%%  Other paths
%% -----------------------------------------------------------------------------

handle_request([], 'GET', _Arg) ->
    redirect_to_base([?APP, ?GUI, ?PUB]);

handle_request([?APP], 'GET', _Arg) ->
    redirect_to_base([?APP, ?GUI, ?PUB]);

handle_request([?APP, ?GUI], 'GET', _Arg) ->
    redirect_to_base([?APP, ?GUI, ?PUB]);

handle_request(["favicon.ico" = FileName], 'GET', _Arg) ->
    serve_priv_file(FileName, yaws_api:mime_type(FileName));

handle_request(_Path, 'GET', Arg) ->
    [
        {status, 404},
        {ehtml, [{p, [], [
            io_lib:format("404: Page ~p not found.", [yaws_api:request_url(Arg)])
        ]}]}
    ];

handle_request(_Path, _Method, _Arg) ->
    [
        {status, 400}
    ].



%% =============================================================================
%%  Helper functions.
%% =============================================================================


redirect_to_base(Base) ->
    {redirect, lists:flatten(["/", string:join(Base, "/"), "/"])}.

serve_index_or_file(Base, Tail, Path, Arg) ->
    case Tail of
        [] ->
            Uri = yaws_api:request_url(Arg),
            case lists:last(Uri#url.path) of
                $/ -> serve_priv_file(string:join(Base ++ ["index.html"], "/"), "text/html");
                _  -> redirect_to_base(Base)
            end;
        _ ->
            [?APP | WwwPath] = Path,
            FileName = string:join(WwwPath, "/"),
            serve_priv_file(FileName, yaws_api:mime_type(FileName))
    end.


serve_priv_file(FileName, ContentType) ->
    {ok, ThisApp} = application:get_application(?MODULE),
    PrivDir = case code:priv_dir(ThisApp) of
        {error,bad_name} -> "priv"; % To allow testing without creating whole app.
        Dir -> Dir
    end,
    AbsolutePath = lists:flatten(PrivDir ++ "/www/" ++ FileName),
    {ok, Content} = file:read_file(AbsolutePath),
    {content, ContentType, Content}.


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
timestamp_to_bin(undefined) ->
    null;

timestamp_to_bin({{Y, M, D}, {H, Mi, S}}) ->
    Args = [Y, M, D, H, Mi, S],
    Date = io_lib:format("~B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ", Args),
    erlang:iolist_to_binary(Date);

timestamp_to_bin(Now) ->
    {_MegaSecs, _Secs, MicroSecs} = Now,
    {{Y, M, D}, {H, Mi, S}} = calendar:now_to_datetime(Now),
    Args = [Y, M, D, H, Mi, S, MicroSecs],
    Date = io_lib:format("~B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0BZ", Args),
    erlang:iolist_to_binary(Date).


%%
%%
%%
to_int(String) when is_list(String) ->
    erlang:list_to_integer(String).


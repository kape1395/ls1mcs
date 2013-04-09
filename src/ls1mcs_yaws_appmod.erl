-module(ls1mcs_yaws_appmod).
-compile([{parse_transform, lager_transform}]).
-export([out/1]).
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
    [
        {status, 200},
        {header, {"Link", "<command>; rel=commands"}},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];


%%
%%  Command resources
%%

handle_request([?APP, ?API, "command"], 'GET', _Arg) ->
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

handle_request([?APP, ?API, "Command", _CommandId], 'GET', _Arg) ->
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
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



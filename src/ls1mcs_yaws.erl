%%
%%  http://stateless.co/hal_specification.html
%%
-module(ls1mcs_yaws).
-compile([{parse_transform, lager_transform}]).
-export([out/1]).
-export([url/2]).
-include("ls1mcs.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-define(APP, "ls1mcs").
-define(API, "api").
-define(GUI, "gui").
-define(PUB, "pub").
-define(MCS, "mcs").
-define(HAM, "ham").
-define(TMP, "tmp").    % Temporary MCS GUI


%% =============================================================================
%%  API functions.
%% =============================================================================

%%
%%  This function is invoked by Yaws.
%%
out(Arg) ->
    Uri = yaws_api:request_url(Arg),
    Path = string:tokens(Uri#url.path, "/"),
    Method = yaws_api:http_request_method(Arg#arg.req),
    lager:info("Handling request: path=~p, method=~p", [Path, Method]),
    handle_request(Path, Method, Arg).



%% =============================================================================
%%  Request handling.
%% =============================================================================

handle_request([?APP, ?API | Tail], Method, Arg) ->
    ls1mcs_yaws_api:handle_request(Tail, Method, Arg);

handle_request([?APP, ?GUI, ?MCS | Tail] = Path, 'GET', Arg) ->
    serve_index_or_file([?GUI, ?MCS], Tail, Path, Arg);

handle_request([?APP, ?GUI, ?PUB | Tail] = Path, 'GET', Arg) ->
    serve_index_or_file([?GUI, ?PUB], Tail, Path, Arg);

handle_request([?APP, ?GUI, ?HAM | Tail] = Path, 'GET', Arg) ->
    serve_index_or_file([?GUI, ?HAM], Tail, Path, Arg);

handle_request([?APP, ?GUI, ?TMP | Tail] = Path, 'GET', Arg) ->
    serve_index_or_file([?GUI, ?TMP], Tail, Path, Arg);


%% -----------------------------------------------------------------------------
%%  Other paths
%% -----------------------------------------------------------------------------

handle_request([], 'GET', _Arg) ->
    redirect_to_base([?APP, ?GUI, ?TMP]);

handle_request([?APP], 'GET', _Arg) ->
    redirect_to_base([?APP, ?GUI, ?TMP]);

handle_request([?APP, ?GUI], 'GET', _Arg) ->
    redirect_to_base([?APP, ?GUI, ?TMP]);

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
url(api, Path) ->
    list_to_binary(string:join(["", ?APP, ?API | Path], "/")).



-module(fiftypm_api_login).

-export(
    [
        init/2
    ]
).

-define(GITHUB_AUTH_URL_PRE, <<"https://github.com/login/oauth/authorize?">>).

-ifdef(DEVELOP).
-define(GITHUB_AUTH_CB_URL, <<"http://127.0.0.1:8000/50pm/api/login/github">>).
-elifdef(STAGING).
-define(GITHUB_AUTH_CB_URL, <<"http://dev.davidhuang.top/50pm/api/login/github">>).
-else.
-define(GITHUB_AUTH_CB_URL, <<"GITHUB_AUTH_CB_URL not properly set">>).
-endif.

%%====================================================================
%% Callback functions
%%====================================================================

init(R, S) ->
    ExtAuth = cowboy_req:binding(oidp, R),
    handle(ExtAuth, R, S).

handle(<<"github">>, R, S) ->
    Q = build_qs(
        #{
            response_type => "code",
            client_id => client_id(),
            redirect_uri => ?GITHUB_AUTH_CB_URL,
            scope => "user public_repo",
            state => "0123456789abcdef"
        }),

    AuthURL = ?GITHUB_AUTH_URL_PRE ++ Q,
    httpres:'200'(AuthURL, R, S).

build_qs(M) ->
    PL = maps:to_list(M),
    SL = lists:map(
            fun({K, V}) -> atom_to_list(K) ++ "=" ++ http_uri:encode(V) end,
            PL),
    QS = lists:join("&", SL),
    lists:flatten(QS).
%%====================================================================
%% Internal functions
%%====================================================================

client_id() -> application:get_env(fiftypm_api, client_id).
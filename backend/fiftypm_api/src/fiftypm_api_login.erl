-module(fiftypm_api_login).

-export(
    [
        init/2
    ]
).

-define(GITHUB_AUTH_URL, "https://github.com/login/oauth/authorize?").
-define(GITHUB_TOKN_URL, "https://github.com/login/oauth/access_token?").


-ifdef(DEVELOP).
    -define(GITHUB_OAUTH_CB_URL, "http://127.0.0.1:8000/50pm/api/login/github/callback").
    -define(APP_URL, "http://127.0.0.1:8080/50pm").
-else.
    -ifdef(STAGING).
        -define(GITHUB_OAUTH_CB_URL, "https://dev.davidhuang.top/50pm/api/login/github/callback").
        -define(APP_URL, "https://dev.davidhuang.top/50pm").
    -else.
        -define(GITHUB_OAUTH_CB_URL, "GITHUB_OAUTH_CB_URL not properly set").
        -define(APP_URL, "APP_URL not properly set").
    -endif.
-endif.

%%====================================================================
%% Callback functions
%%====================================================================

init(R, S) ->
    PathList = cowboy_req:path_info(R),
    #{session := Session} = cowboy_req:match_cookie([{session, [], undefined}]),
    handle(Session, PathList, R, S).

%%====================================================================
%% Internal functions
%%====================================================================

handle(_Session, [Provider], R, S) ->  oauth_start(Provider, R, S);
handle(Session, [Provider, <<"callback">>], R, S) -> oauth_callback(Session, Provider, R, S).

oauth_start(<<"github">>, R, S) -> 
    Q = build_qs(
        #{
            response_type => "code",
            client_id => fiftypm_api_env:client_id(),
            redirect_uri => ?GITHUB_OAUTH_CB_URL,
            scope => "user",
            state => binary_to_list(oauth_new_state(github, fiftypm_api_env:oauth_state_timeout()))
        }
    ),
    AuthURL = ?GITHUB_AUTH_URL ++ Q,
    httpres:'302'(AuthURL, R, S).

oauth_callback(Session, Provider, R, S) ->
    #{code := Code, state := StateInt} 
        = cowboy_req:match_qs(
            [
                {code, nonempty, undefined}, 
                {state, nonempty, undefined}
            ],
            R
        ),
    oauth_callback(Session, Provider, Code, StateInt, R, S).

oauth_callback(_Session, _Provider, undefined, _State, R, S) -> httpres:'400'("oauth callback - no code", R, S);
oauth_callback(_Session, _Provider, _Code, undefined, R, S) -> httpres:'400'("oauth callback - no state", R, S);
oauth_callback(Session, <<"github">>, Code, State, R, S) ->
    ChkStateRet = oauth_chk_state(State, github),
    oauth_callback_github(Session, ChkStateRet, Code, State, R, S).

oauth_callback_github(_Session, false, _Code, _State, R, S) -> httpres:'400'("oauth callback - bad state", R, S);
oauth_callback_github(Session, true, Code, State, R, S) ->
    State = binary_to_list(State),
    Q = build_qs(
        #{
            client_id => fiftypm_api_env:client_id(),
            client_secret => fiftypm_api_env:client_secret(),
            code => binary_to_list(Code),
            redirect_uri => ?GITHUB_OAUTH_CB_URL,
            state => State
        }
    ),
    TokenURL = ?GITHUB_TOKN_URL ++ Q,
    {ok, {{_, 200, _}, Headers, Body}} = httpc:request(post, {TokenURL, [], [], []}, [], []),
    io:format("~noauth_callback_github Headers ~p", [Headers]),
    io:format("~noauth_callback_github Body ~p~n", [Body]),
    tab_kv:ddel(Session),
    tab_kv:dset(State, {session, #{atoken => Body}}), % parsing body to get access token missing here
    R1 = cowboy_req:set_resp_cookie(<<"session">>, State, R, session_cookie_opts()),
    httpres:'302'(?APP_URL, R1, S).

-define(DEFAULT_STATE_LEN, 8).
oauth_new_state(Opaque, TimeOut) -> 
    State = base64:encode(crypto:strong_rand_bytes(?DEFAULT_STATE_LEN)),
    tab_kv:dset(State, {oauth_state, Opaque}),
    _Pid = spawn(fun() -> timer:sleep(TimeOut), tab_kv:ddel(State) end),
    State.

oauth_chk_state(State, V) when is_integer(State) -> oauth_chk_state(State, V, tab_kv:dget(State)).
oauth_chk_state(State, V, {State, {oauth_state, V}}) -> true;
oauth_chk_state(_, _, _) -> false.

%%====================================================================
%% Untility functions
%%====================================================================

build_qs(M) ->
    PL = maps:to_list(M),
    SL = lists:map(
            fun({K, V}) -> atom_to_list(K) ++ "=" ++ http_uri:encode(V) end,
            PL),
    QS = lists:join("&", SL),
    lists:flatten(QS).

-define(DEFAULT_SESSION_COOKIE_OPTS, #{max_age => 30}).
session_cookie_opts() -> ?DEFAULT_SESSION_COOKIE_OPTS.
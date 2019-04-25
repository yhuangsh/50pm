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
    PL = cowboy_req:path_info(R),
    handle(PL, R, S).

handle([Provider], R, S) -> oauth_start(Provider, R, S);
handle([Provider, <<"callback">>], R, S) -> 
    #{code := Code, state := State} 
        = cowboy_req:match_qs(
            [
                {code, nonempty, undefined}, 
                {state, int, undefined}
            ],
            R
        ),
    oauth_callback(Provider, Code, State, R, S).

oauth_start(<<"github">>, R, S) ->
    Q = build_qs(
        #{
            response_type => "code",
            client_id => client_id(),
            redirect_uri => ?GITHUB_OAUTH_CB_URL,
            scope => "user",
            state => integer_to_list(gen_state(github, state_timeout()))
        }
    ),
    AuthURL = ?GITHUB_AUTH_URL ++ Q,
    httpres:'302'(AuthURL, R, S).

oauth_callback(_Provider, undefined, _State, R, S) -> httpres:'400'("no code", R, S);
oauth_callback(_Provider, _Code, undefined, R, S) -> httpres:'400'("bad state", R, S);
oauth_callback(<<"github">>, Code, State, R, S) ->
    Q = build_qs(
        #{
            client_id => client_id(),
            client_secret => client_secret(),
            code => binary_to_list(Code),
            redirect_uri => ?GITHUB_OAUTH_CB_URL,
            state => integer_to_list(State)
        }
    ),
    TokenURL = ?GITHUB_TOKN_URL ++ Q,
    {ok, {{_, 200, _}, Headers, Body}} = httpc:request(post, {TokenURL, [], [], []}, [], []),
    io:format("~noauth_callback Headers ~p", [Headers]),
    io:format("~noauth_callback Body ~p~n", [Body]),
    httpres:'302'(?APP_URL, R, S).

%%====================================================================
%% Internal functions
%%====================================================================

build_qs(M) ->
    PL = maps:to_list(M),
    SL = lists:map(
            fun({K, V}) -> atom_to_list(K) ++ "=" ++ http_uri:encode(V) end,
            PL),
    QS = lists:join("&", SL),
    lists:flatten(QS).

client_id() -> client_id(application:get_env(fiftypm_api, client_id)).
client_id({ok, ClientId}) -> ClientId.

client_secret() -> client_secret(application:get_env(fiftypm_api, client_secret)).
client_secret({ok, ClientSecret}) -> ClientSecret.

gen_state(V, TimeOut) -> 
    StateInt = rand:uniform(1000000),
    tab_kv:dset(StateInt, {oauth_state, V}),
    _Pid = spawn(fun() -> timer:sleep(TimeOut), tab_kv:ddel(StateInt) end),
    StateInt.
    
-define(DEFAULT_STATE_TIMEOUT, (10*1000)).
state_timeout() -> application:get_env(fiftypm_api, state_timeout, ?DEFAULT_STATE_TIMEOUT).
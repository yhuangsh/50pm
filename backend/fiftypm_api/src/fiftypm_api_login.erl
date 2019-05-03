-module(fiftypm_api_login).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/2]).

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
    login(Session, PathList, R, S).

%%====================================================================
%% Internal functions
%%====================================================================

login(_Session, [Provider], R, S) ->  oauth_start(Provider, R, S);
login(Session, [Provider, <<"callback">>], R, S) -> oauth_callback(Session, Provider, R, S).

oauth_start(<<"github">>, R, S) -> 
    Params = 
        build_qs(
            #{
                response_type => "code",
                client_id => fiftypm_api_env:oauth_client_id(),
                redirect_uri => ?GITHUB_OAUTH_CB_URL,
                scope => "user",
                state => integer_to_list(oauth_new_state(github, fiftypm_api_env:oauth_state_timeout()))
            }
        ),
    AuthURL = ?GITHUB_AUTH_URL ++ Params,
    httpres:'302'(AuthURL, R, S).

oauth_callback(Session, Provider, R, S) ->
    #{code := Code, state := StateInt} =
        cowboy_req:match_qs(
            [
                {code, nonempty, undefined}, 
                {state, int, undefined}
            ],
            R
        ),
    oauth_callback(Session, Provider, Code, StateInt, R, S).
oauth_callback(_Session, _Provider, undefined, _StateInt, R, S) -> httpres:'400'("oauth callback - no code", R, S);
oauth_callback(_Session, _Provider, _Code, undefined, R, S) -> httpres:'400'("oauth callback - no state", R, S);
oauth_callback(Session, <<"github">>, Code, StateInt, R, S) ->
    ChkStateRet = oauth_chk_state(StateInt, github),
    oauth_callback_github(Session, ChkStateRet, Code, StateInt, R, S).

oauth_callback_github(_Session, false, _Code, _StateInt, R, S) -> httpres:'400'("oauth callback - bad state", R, S);
oauth_callback_github(Session, true, Code, StateInt, R, S) ->
    State = integer_to_list(StateInt),
    Params = 
        build_qs(
            #{
                client_id => fiftypm_api_env:oauth_client_id(github),
                code => binary_to_list(Code),
                redirect_uri => ?GITHUB_OAUTH_CB_URL,
                state => State
            }
        ),
    AuthHeader = {"Authorization", "Basic " ++ fiftypm_api_env:oauth_client_secret(github)},
    ContentType = "application/x-www-form-urlencoded",
    TokenURL = ?GITHUB_TOKN_URL,
    {ok, {{_, 200, _}, Headers, Body}} = 
        httpc:request(
            post, 
            {TokenURL, [AuthHeader], [ContentType], Params}, 
            [], 
            []
        ),
    io:format("~noauth_callback_github Headers ~p", [Headers]),
    io:format("~noauth_callback_github Body ~p~n", [Body]),
    del_session(Session),
    SessionData =
        cowboy_req:match_qs(
            [
                {access_token, nonempty, undefined},
                {scope, nonempty, undefined},
                {token_type, nonempty, <<"bearer">>}
            ], 
            #{qs => Body}
        ),
    {NewSession, SessionCookieOpts} = new_session(SessionData),
    R1 = cowboy_req:set_resp_cookie(<<"session">>, NewSession, R, SessionCookieOpts),
    httpres:'302'(?APP_URL, R1, S).

%% state
oauth_new_state(Opaque, TimeOut) -> 
    StateInt = rand:uniform(fiftypm_api_env:oauth_state_domain()),
    kv:dset(oauth_state, StateInt, Opaque),
    _Pid = spawn(fun() -> timer:sleep(TimeOut), kv:ddel(oauth_state, StateInt) end),
    StateInt.

oauth_chk_state(StateInt, Opaque) when is_integer(StateInt) -> oauth_chk_state(StateInt, Opaque, kv:dget(oauth_state, StateInt)).
oauth_chk_state(StateInt, Opaque, {StateInt, Opaque}) -> true;
oauth_chk_state(_, _, _) -> false.

%% session
del_session(undefined) -> ok;
del_session(SessionId) when is_binary(SessionId) -> kv:ddel(session, SessionId).

get_session(SessionId) -> kv:dget(session, SessionId).

new_session(SessionData) -> new_session(SessionData, session_cookie_opts()).
new_session(SessionData, SessionCookieOpts) when is_map(SessionData) ->
    SessionId = gen_session_id(),
    Atime = Ctime = calendar:universal_time(),
    kv:dset(
        session, 
        SessionId, 
        SessionData#{
            ctime => Ctime, 
            atime => Atime, 
            cookie_opts => SessionCookieOpts
        }
    ),
    {SessionId, SessionCookieOpts}.

-define(DEFAULT_SESSION_ID_LEN, 8).
gen_session_id() -> base64:encode(crypto:strong_rand_bytes(?DEFAULT_SESSION_ID_LEN)).

-define(DEFAULT_SESSION_COOKIE_OPTS, #{max_age => 30}).
session_cookie_opts() -> ?DEFAULT_SESSION_COOKIE_OPTS.

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

%%====================================================================
%% Unit tests
%%====================================================================

-ifdef(TEST).

login_test_() ->
    {
        setup, 
        fun setup/0, 
        fun cleanup/1, 
        fun (_D) ->
            [
                test_oauth_state(),
                test_session()
            ] 
        end
    }.

setup() ->
    mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    {atomic, ok} = kv:create_table([oauth_state, session]),
    ok.

cleanup(_) -> 
    {atomic, ok} = mnesia:delete_table(oauth_state),
    {atomic, ok} = mnesia:delete_table(session),
    stopped = mnesia:stop().

test_oauth_state() ->    
    State = oauth_new_state("oauth_state", 50), 
    ChkStateRetBeforeTimout = oauth_chk_state(State, "oauth_state"),
    timer:sleep(60),
    ChkStateRetAfterTimout = oauth_chk_state(State, "oauth_state"),
    [
        ?_assertMatch(true, is_integer(State)),
        ?_assertMatch(true, ChkStateRetBeforeTimout),
        ?_assertMatch(false, ChkStateRetAfterTimout)
    ].

test_session() ->
    {SessionId, _CookieOpts} = new_session(#{test_data => "session data"}),
    {SessionId2, SessionData} = get_session(SessionId),
    ok = del_session(SessionId),
    Undefined = get_session(SessionId),
    [
        ?_assertMatch(
            true, 
            maps:is_key(test_data, SessionData) andalso
                maps:is_key(atime, SessionData) andalso
                maps:is_key(ctime, SessionData) andalso
                maps:is_key(cookie_opts, SessionData)
        ),
        ?_assertMatch(SessionId, SessionId2),
        ?_assertMatch(undefined, Undefined)
    ].
-endif.


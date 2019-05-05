-module(oauth).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export(
    [
        create_table/0, 
        add_table_copy/0,

        initiate/2,
        callback/2
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
%% API
%%====================================================================

create_table() -> kv:create_table(oauth_state).
add_table_copy() -> kv:add_table_copy(oauth_state).

initiate(<<"github">>, _QueryStr) -> 
    Params = 
        build_qs(
            #{
                response_type => "code",
                client_id => fiftypm_api_env:oauth_client_id(github),
                redirect_uri => ?GITHUB_OAUTH_CB_URL,
                scope => "user",
                state => integer_to_list(new_state(github))
            }
        ),
    _AuthURL = ?GITHUB_AUTH_URL ++ Params.

callback(<<"github">>, QueryStr) ->
    #{code := Code, state := StateInt} =
        cowboy_req:match_qs(
            [
                {code, nonempty, undefined}, 
                {state, int, undefined}
            ],
            #{qs => QueryStr}
        ),
    callback_github(Code, StateInt).

callback_github(undefined, _StateInt) -> {error, "oauth:callback - no code"};
callback_github(_Code, undefined) -> {error, "oauth:callback - no state"};
callback_github(Code, StateInt) -> callback_github(chk_state(StateInt, github), Code, StateInt).
callback_github(false, _Code, _StateInt) -> {error, "oauth:callback - bad state"};
callback_github(true, Code, StateInt) ->
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
    io:format("~noauth:callback_github Headers ~p", [Headers]),
    io:format("~noauth:callback_github Body ~p~n", [Body]),
    SessionData =
        cowboy_req:match_qs(
            [
                {access_token, nonempty},
                {scope, nonempty},
                {token_type, nonempty}
            ], 
            #{qs => Body}
        ),
    {ok, ?APP_URL, SessionData}.

%%====================================================================
%% Internal functions
%%====================================================================

new_state(Opaque) -> new_state(Opaque, fiftypm_api_env:oauth_state_timeout()).
new_state(Opaque, TimeOut) -> 
    StateInt = rand:uniform(fiftypm_api_env:oauth_state_domain()),
    kv:dset(oauth_state, StateInt, Opaque),
    _Pid = spawn(fun() -> timer:sleep(TimeOut), kv:ddel(oauth_state, StateInt) end),
    StateInt.

chk_state(StateInt, Opaque) when is_integer(StateInt) -> chk_state(StateInt, Opaque, kv:dget(oauth_state, StateInt)).
chk_state(StateInt, Opaque, {StateInt, Opaque}) -> true;
chk_state(_, _, _) -> false.

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
                test_oauth_state()
            ] 
        end
    }.

setup() ->
    mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    {atomic, ok} = create_table(),
    ok.

cleanup(_) -> 
    {atomic, ok} = mnesia:delete_table(oauth_state),
    stopped = mnesia:stop().

test_oauth_state() ->    
    State = new_state("oauth_state", 50), 
    ChkStateRetBeforeTimout = chk_state(State, "oauth_state"),
    timer:sleep(60),
    ChkStateRetAfterTimout = chk_state(State, "oauth_state"),
    [
        ?_assertMatch(true, is_integer(State)),
        ?_assertMatch(true, ChkStateRetBeforeTimout),
        ?_assertMatch(false, ChkStateRetAfterTimout)
    ].

-endif.


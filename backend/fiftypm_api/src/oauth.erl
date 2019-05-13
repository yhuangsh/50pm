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
                client_id => github_client_id(),
                redirect_uri => github_auth_callback_url(),
                scope => "user",
                state => integer_to_list(new_state(github))
            }
        ),
    _AuthURL = github_auth_url() ++ Params.

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

%%====================================================================
%% Internal functions
%%====================================================================

callback_github(undefined, _StateInt) -> {error, "oauth:callback - no code"};
callback_github(_Code, undefined) -> {error, "oauth:callback - no state"};
callback_github(Code, StateInt) -> callback_github(chk_state(StateInt, github), Code, StateInt).
callback_github(false, _Code, _StateInt) -> {error, "oauth:callback - bad state"};
callback_github(true, Code, StateInt) ->
    State = integer_to_list(StateInt),
    Params = 
        build_qs(
            #{
                client_id => github_client_id(),
                code => binary_to_list(Code),
                redirect_uri => github_auth_callback_url(),
                state => State
            }
        ),
    AuthHeader = {"Authorization", "Basic " ++ github_client_secret()},
    ContentType = "application/x-www-form-urlencoded",
    TokenURL = github_token_url(),
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
    {ok, github_app_url(), SessionData}.


%% OAuth state
new_state(Opaque) -> new_state(Opaque, state_timeout()).
new_state(Opaque, TimeOut) -> 
    StateInt = rand:uniform(state_domain()),
    kv:dset(oauth_state, StateInt, Opaque),
    _Pid = spawn(fun() -> timer:sleep(TimeOut), kv:ddel(oauth_state, StateInt) end),
    StateInt.

chk_state(StateInt, Opaque) when is_integer(StateInt) -> chk_state(StateInt, Opaque, kv:dget(oauth_state, StateInt)).
chk_state(StateInt, Opaque, {StateInt, Opaque}) -> true;
chk_state(_, _, _) -> false.

%% OAuth settings
state_domain() -> get_env(oauth_state_domain).
state_timeout() -> get_env(oauth_state_timeout).

%% OAuth settings - Github
github_client_id() -> get_env(oauth_github_client_id).
github_client_secret() -> get_env(oauth_github_client_secret).

github_auth_url() -> get_env(oauth_github_auth_url). 
github_token_url() -> get_env(oauth_github_token_url). 

-ifdef(DEVELOP).
github_auth_callback_url() -> get_env(oauth_github_auth_callback_url_develop).
github_app_url() -> get_env(oauth_github_app_url_develop).
-else.
    -ifdef(STAGING).
github_auth_callback_url() -> get_env(oauth_github_auth_callback_url_staging).
github_app_url() -> get_env(oauth_github_app_url_staging).
    -else.
        -ifdef(RELEASE).
github_auth_callback_url() -> get_env(oauth_github_auth_callback_url_release).
github_app_url() -> get_env(oauth_github_app_url_release).
        -else.
github_auth_callback_url() -> throw(build_option_not_defined).
github_app_url() -> throw(build_option_not_defined).
        -endif.
    -endif.
-endif.

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

get_env(Env) -> get_env(Env, undefined).
get_env(Env, Default) -> application:get_env(fiftypm_api, Env, Default).

%%====================================================================
%% Unit tests
%%====================================================================

-ifdef(TEST).

oauth_test_() ->
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


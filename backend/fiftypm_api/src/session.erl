-module(session).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export(
    [
        create_table/0, 
        add_table_copy/0,

        new/1,
        get/1, 
        delete/1
    ]
).

%%====================================================================
%% API
%%====================================================================

create_table() -> {atomic, ok} = kv:create_table(session).
add_table_copy() -> kv:add_table_copy(session).

delete(undefined) -> ok;
delete(SessionId) when is_binary(SessionId) -> kv:ddel(session, SessionId).

get(SessionId) -> kv:dget(session, SessionId).

new(SessionData) -> new(SessionData, session_cookie_opts()).
new(SessionData, SessionCookieOpts) when is_map(SessionData) ->
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
%% Unit tests
%%====================================================================

-ifdef(TEST).

session_test_() ->
    {
        setup, 
        fun setup/0, 
        fun cleanup/1, 
        fun (_D) ->
            [   
                test_session()
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
    {atomic, ok} = mnesia:delete_table(session),
    stopped = mnesia:stop().

test_session() ->
    {SessionId, _CookieOpts} = new(#{test_data => "session data"}),
    {SessionId2, SessionData} = session:get(SessionId),
    ok = delete(SessionId),
    Undefined = session:get(SessionId),
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
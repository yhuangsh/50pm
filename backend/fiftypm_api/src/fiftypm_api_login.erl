-module(fiftypm_api_login).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/2]).

%%====================================================================
%% Callback functions
%%====================================================================

init(R, S) ->
    PathList = cowboy_req:path_info(R),
    #{session := OldSession} = cowboy_req:match_cookie([{session, [], undefined}]),
    login(OldSession, PathList, R, S).

%%====================================================================
%% Internal functions
%%====================================================================

login(_OldSessionId, [Provider], R, S) ->  httpres:'302'(oauth:initiate(Provider), R, S);
login(OldSessionId, [Provider, <<"callback">>], R, S) -> start_new_session(oauth:callback(Provider), OldSessionId, R, S).

start_new_session({error, Msg}, _OldSessionId, R, S) -> httpres:'400'(Msg, R, S);
start_new_session({AppURL, OAuthRes}, OldSessionId, R, S) when is_list(AppURL), is_map(OAuthRes) ->
    session:delete(OldSessionId),
    {NewSession, SessionCookieOpts} = session:new(OAuthRes),
    R1 = cowboy_req:set_resp_cookie(<<"session">>, NewSession, R, SessionCookieOpts),
    httpres:'302'(AppURL, R1, S).

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

-endif.


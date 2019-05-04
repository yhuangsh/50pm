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
    #{session := Session} = cowboy_req:match_cookie([{session, [], undefined}]),
    login(Session, PathList, R, S).

%%====================================================================
%% Internal functions
%%====================================================================

login(_Session, [Provider], R, S) ->  oauth:initiate(Provider, R, S);
login(Session, [Provider, <<"callback">>], R, S) -> oauth:callback(Session, Provider, R, S).

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


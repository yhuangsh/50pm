-module(fiftypm_api_session).

-export([init/2,
         allowed_methods/2,
         content_types_provided/2]).

-export([json_providers/2]).

-define(CT_JSON, {<<"application">>, <<"json">>, '*'}).

%%====================================================================
%% Callback functions
%%====================================================================

init(R, S) -> 
    Method = cowboy_req:method(R),
    #{sid := Sid} = cowboy_req:match_cookies([{sid, [], <<"undefined">>}], R),
    {cowboy_rest, R, S#{method => Method, sid => Sid}}.

allowed_methods(R, S) -> {[<<"GET">>], R, S}.

content_types_provided(R, S) -> {[{?CT_JSON, json_providers}], R, S}.

%%====================================================================
%% Internal functions
%%====================================================================

%% Providers
json_providers(R, S = #{method := M}) -> json_providers(M, R, S).
json_providers(<<"GET">>, R, S) -> 'GET /50pm/api/v1/session'(R, S).

%% Read/CRUD
'GET /50pm/api/v1/session'(R, S = #{sid := Sid}) -> 
    Session = tab_session:read(Sid),
    get_session(Session, R, S).

get_session([], R, S) ->
    Sdata = new_session_data(),
    Sid = tab_session:create(Sdata),
    R1 = cowboy_req:set_resp_cookie(<<"sid">>, Sid, R, new_cookie_opts()),
    get_session([{session, Sid, Sdata}], R1, S);
get_session([{session, _Sid, Sdata}], R, S) ->
    SavedSettings = maps:with(settings(), Sdata),
    Ret = #{
        <<"success">> => true,
        <<"server_version">> => <<"v1">>,
        <<"saved_settings">> => SavedSettings
    },
    {jsx:encode(Ret), R, S}.

new_session_data() ->
    Ctime = calendar:universal_time(),
    Atime = Ctime,
    #{
        ctime => Ctime,
        atime => Atime,
        calc_opt => [true, false],
        digit_opt => [true, false, false],
        unknown_opt => [true, false, false],
        page_opt => [true, false, false, false]
    }.

new_cookie_opts() ->
    #{
        max_age => 3600*24*365,
        secure => true,
        http_only => true
    }.

settings() -> [calc_opt, digit_opt, unknown_opt, page_opt].

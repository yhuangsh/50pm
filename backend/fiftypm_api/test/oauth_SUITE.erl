-module(oauth_SUITE).

-include_lib("common_test/include/ct.hrl").
-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).
-export([test_oauth_initiate_github/1]).
 
-define(TIMEOUT, 5000).
-define(HOST, "http://127.0.0.1").
-define(PORT, "80").

suite() -> 
    [
        {require, username}, 
        {require, password}
    ].

all() -> [test_oauth_initiate_github].
 
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ssl),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(fiftypm_api),  
    io:format("fiftypm_api env=~p~n", [application:get_all_env(fiftypm_api)]),
    io:format("which_applications=~p~n", [application:which_applications()]),
    Config.

end_per_suite(_Config) -> ok.

%%====================================================================
%% Tests
%%====================================================================

%% /api0/v1/version
test_oauth_initiate_github(_Config) -> 
    %% HTTP Request 0 - User clicks login button on 50pm
    L0 = "http://127.0.0.1/50pm/api/login/github",
    io:format("~n*** HTTP request 0, URL = ~s", [L0]),
    H0in = [],
    {C0out, H0out, D0out} = 'GET'(L0, H0in),
    io:format("~nC0out = ~p~nH0out = ~p~nD0out = ~s", [C0out, H0out, D0out]),
   
    %% HTTP Request 1 - 50pm back end redirects user to Github oauth authorization endpoint
    L1 = proplists:get_value("location", H0out),
    io:format("~n*** HTTP request 1, URL = ~s", [L1]),
    H1in = combine_cookies(H0out),
    {C1out, H1out, D1out} = 'GET'(L1, H1in),
    io:format("~nC1out = ~p~nH1out = ~p~nD1out = ~s", [C1out, H1out, D1out]),

    %% HTTP Request 2 - Detecting no active login (this test has nothing to do with browser session/cookie data), 
    %% redirecting to login page
    L2 = proplists:get_value("location", H1out),
    io:format("~n*** HTTP request 2, URL = ~s", [L2]),
    H2in = combine_cookies(H1out),
    {C2out, H2out, D2out} = 'GET'(L2, H2in),
    io:format("~nC2out = ~p~nH2out = ~p~nD2out = ~s", [C2out, H2out, "...Github Login website (lots of HTMLs)..."]),

    %% HTTP Request 3 - Parsing the login page and emulate a login form submission
    {match, FormLocations} = re:run(D2out, "<form .+>", [global, multiline, ungreedy, unicode, {capture, all, list}]),
    io:format("~nForm = ~p", [FormLocations]),
    FormElements = lists:map(fun([S]) -> S end, FormLocations),
    io:format("~nFormElement = ~p", [FormElements]),
    FormAttrs = lists:map(fun(E) -> filter_form_attrs(E) end, FormElements),
    [LoginFormAttrs] = FormAttrs,
    io:format("~nFormAttrs = ~p", [FormAttrs]),
    {match, InputLocations} = re:run(D2out, "<input .+>", [global, multiline, ungreedy, unicode, {capture, all, list}]),
    io:format("~nInputLocations = ~p", [InputLocations]),
    InputElements = lists:map(fun([S]) -> S end, InputLocations),
    io:format("~nInputElements = ~p", [InputElements]),
    InputAttrs = lists:map(fun(E) -> filter_input_attrs(E) end, InputElements),
    io:format("~nInputAttrs = ~p", [InputAttrs]),
    ParamList0 = lists:filtermap(fun(E) -> convert_to_name_value(E) end, InputAttrs),
    Username = ct:get_config(username),
    Password = ct:get_config(password),
    ParamList = ParamList0 ++ [{"login", Username}, {"password", Password}],
    io:format("~nParamsList = ~p", [ParamList]),
    QueryStr = uri_string:compose_query(ParamList),
    io:format("~nQueryStr = ~p", [QueryStr]),
    {ok, {Scheme, _UserInfo, Host, _Port, _Path, _Query}} = http_uri:parse(L2),
    L3 = atom_to_list(Scheme) ++ "://" ++ Host ++ proplists:get_value(action, LoginFormAttrs),
    io:format("~n*** HTTP request 3, URL = ~p", [L3]),
    H3in = combine_cookies(H2out),
    D3in = QueryStr,
    {C3out, H3out, D3out} = 'POST'(L3, H3in, D3in),
    io:format("~nC3out = ~p~nH3out = ~p~nD3out = ~s", [C3out, H3out, D3out]),

    %% HTTP Request 4 - Login successful, then redirectted again back to 
    %% Github's authorization endpoint
    L4 = proplists:get_value("location", H3out),
    io:format("~n*** HTTP request 4, URL = ~s", [L4]),
    H4in = combine_cookies(H3out),
    {C4out, H4out, D4out} = 'GET'(L4, H4in),
    io:format("~nC4out = ~p~nH4out = ~p~nD4out = ~s", [C4out, H4out, D4out]),

    %% HTTP Request 5 - Code request successful, then redirectted back to 
    %% 50pm backend url
    L5 = proplists:get_value("location", H4out),
    io:format("~n*** HTTP request 5, URL = ~s", [L5]),
    H5in = combine_cookies(H4out),
    {C5out, H5out, D5out} = 'GET'(L5, H5in),
    io:format("~nC5out = ~p~nH5out = ~p~nD5out = ~s", [C5out, H5out, D5out]).


%% Combine multiple received "set-cookie" values into one 
%% "cookie" header to be sent back in the next http call
combine_cookies(Headers) -> combine_cookies_1(proplists:lookup_all("set-cookie", Headers)).
combine_cookies_1([]) -> [];
combine_cookies_1(CookieHeaders) ->
    CookieList = proplists:lookup_all("set-cookie", CookieHeaders),
    Combined = lists:foldl(fun({_, V}, A) -> A ++ V ++ "; " end, [], CookieList),
    io:format("~ncombined cookie string = ~p", [Combined]),
    [{"cookie", Combined}].

%% Extract <input> element's attributes
%% only name, type, value are extracted
filter_form_attrs(E) ->
    {match, [Action]} = re:run(E, "action=\".*\"", [unicode, ungreedy, {capture, all, list}]),
    {match, [AcceptCharset]} = re:run(E, "accept-charset=\".*\"", [unicode, ungreedy, {capture, all, list}]),
    {match, [Method]} = re:run(E, "method=\".*\"", [unicode, ungreedy, {capture, all, list}]),
    [
        {action, extract_action(Action)}, 
        {accept_charset, extract_accept_charset(AcceptCharset)},
        {method, extract_method(Method)}
    ].

extract_action("action=\"" ++ A) -> [Action, []] = string:split(A, "\""), Action.
extract_accept_charset("accept-charset=\"" ++ AC) -> [AcceptCharset, []] = string:split(AC, "\""), AcceptCharset.
extract_method("method=\"" ++ M) -> [Method, []] = string:split(M, "\""), Method.

filter_input_attrs(E) ->
    {match, [Name]} = re:run(E, "name=\".*\"", [unicode, ungreedy, {capture, all, list}]),
    ValueAttr = re:run(E, "value=\".*\"", [unicode, ungreedy, {capture, all, list}]),
    TypeAttr = re:run(E, "type=\".*\"", [unicode, ungreedy, {capture, all, list}]),
    [{name, extract_name(Name)} | filter_input_attrs(ValueAttr, TypeAttr)].

filter_input_attrs(nomatch, nomatch) -> [];
filter_input_attrs(nomatch, {match, [Type]}) -> [{type, extract_type(Type)}];
filter_input_attrs({match, [Value]}, nomatch) -> [{value, extract_value(Value)}];
filter_input_attrs({match, [Value]}, {match, [Type]}) -> [{value, extract_value(Value)}, {type, extract_type(Type)}].

extract_name("name=\"" ++ N) -> [Name, []] = string:split(N, "\""), Name.
extract_value("value=\"" ++ V) -> [Value, []] = string:split(V, "\""), Value.
extract_type("type=\"" ++ T) -> [Type, []] = string:split(T, "\""), Type.

%% Convert input attributes to {"name", "value"} pairs
convert_to_name_value(L) ->
    Name = proplists:get_value(name, L),
    Value = proplists:get_value(value, L),
    convert_to_name_value(Name, Value).
convert_to_name_value(undefined, _Value) -> false;
convert_to_name_value(_Name, undefined) -> false;
convert_to_name_value("commit", _Value) -> false;
convert_to_name_value(Name, Value) -> {true, {Name, Value}}.

%%====================================================================
%% Internal functions
%%====================================================================

'POST'(Path, HeadersIn, BodyIn) ->
    {ok, {{_, Code, _}, HeadersOut, Body}} = 
        httpc:request(post, {Path, HeadersIn, "application/x-www-form-urlencoded", BodyIn}, 
            [{timeout, ?TIMEOUT}, {autoredirect, false}], []),
    {Code, HeadersOut, Body}.

'GET'(Path, HeadersIn) ->
    {ok, {{_, Code, _}, HeadersOut, BodyOut}} = 
        httpc:request(get, {Path, HeadersIn}, 
            [{timeout, ?TIMEOUT}, {autoredirect, false}], []),
    {Code, HeadersOut, BodyOut}.


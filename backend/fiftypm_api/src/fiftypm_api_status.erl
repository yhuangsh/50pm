-module(fiftypm_api_status).

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
    {cowboy_rest, R, S#{method => Method}}.

allowed_methods(R, S) -> {[<<"GET">>], R, S}.

content_types_provided(R, S) -> {[{?CT_JSON, json_providers}], R, S}.

%%====================================================================
%% Internal functions
%%====================================================================

%% Providers
json_providers(R, S = #{method := M}) -> json_providers(M, R, S).
json_providers(<<"GET">>, R, S) -> 'GET /50pm/api/v1/status'(R, S).

%% Read/CRUD
'GET /50pm/api/v1/status'(R, S) -> 
    Ret = #{
        <<"success">> => true,
        <<"server_version">> => <<"v1">>
    },
    {jsx:encode(Ret), R, S}.
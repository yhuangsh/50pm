-module(fiftypm_api_env).

-behaviour(application).

%% Application callbacks
-export(
    [
        prefix/1,
        client_id/0, client_secret/0,
        oauth_state_timeout/0
    ]
).

%%====================================================================
%% Macros
%%====================================================================

-define(DEFAULT_OAUTH_STATE_TIMEOUT, (10*1000)).

%%====================================================================
%% API
%%====================================================================

prefix(Path) -> application:get_env(fiftypm_api, prefix, "") ++ Path.

client_id() -> client_id(application:get_env(fiftypm_api, client_id)).
client_id({ok, ClientId}) -> ClientId.

client_secret() -> client_secret(application:get_env(fiftypm_api, client_secret)).
client_secret({ok, ClientSecret}) -> ClientSecret.

oauth_state_timeout() -> application:get_env(fiftypm_api, oauth_state_timeout, ?DEFAULT_OAUTH_STATE_TIMEOUT).

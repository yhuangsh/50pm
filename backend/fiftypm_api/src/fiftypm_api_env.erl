-module(fiftypm_api_env).

%% Application callbacks
-export(
    [
        prefix/1,
        
        oauth_client_id/1, 
        oauth_client_secret/1,
        oauth_state_domain/0,
        oauth_state_timeout/0
    ]
).

%%====================================================================
%% Macros
%%====================================================================

-define(DEFAULT_STATE_DOMAIN, 1000000000).
-define(DEFAULT_OAUTH_STATE_TIMEOUT, (10*1000)).

%%====================================================================
%% API
%%====================================================================

prefix(Path) -> application:get_env(fiftypm_api, prefix, "") ++ Path.

oauth_client_id(github) -> oauth_client_id(application:get_env(fiftypm_api, oauth_github_client_id));
oauth_client_id({ok, ClientId}) -> ClientId.

oauth_client_secret(github) -> oauth_client_secret(application:get_env(fiftypm_api, oauth_github_client_secret));
oauth_client_secret({ok, ClientSecret}) -> ClientSecret.

oauth_state_domain() -> application:get_env(fiftypm_api, oauth_state_domain, ?DEFAULT_STATE_DOMAIN).
oauth_state_timeout() -> application:get_env(fiftypm_api, oauth_state_timeout, ?DEFAULT_OAUTH_STATE_TIMEOUT).

-module(fiftypm_api_badreq).

-export([init/2]).

%%====================================================================
%% Callbacks
%%====================================================================

init(R, S) -> httpres:'400'(R, S).
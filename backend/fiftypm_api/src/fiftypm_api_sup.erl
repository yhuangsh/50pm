%%%-------------------------------------------------------------------
%% @doc fiftypm top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(fiftypm_api_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, [child_spec_main()]} }.

%%====================================================================
%% Internal functions
%%====================================================================

child_spec_main() ->
    #{id => fiftypm_api_main, 
      start => {fiftypm_api_main, start_link, []}}.
%%%-------------------------------------------------------------------
%% @doc fiftypm public API
%% @end
%%%-------------------------------------------------------------------

-module(fiftypm_api_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% Macros
%%====================================================================

%% COHOST_STAGING means we deploy the staging services to the same
%% K8s cluster where thr production services are also deployed
%% This is for me to save money.  
%% If COHOST_STAGING is defined, the K8s deployment and service names
%% for the staging services will have to be different than the normal 
%% one.
%% In normal uses, the staging cluster should be a separate cluster 
%% from the production one, therefore using different name is not 
%% necessary or even a bad practice.
%% COHOST_STAGING is defined is the cohost_staging profile is invoked
%% by: rebar3 as cohost_staging ...
-ifdef(COHOST_STAGING).
-define(K8S_SVC_NAME, "fiftypm_api-stg").
-else.
-define(K8S_SVC_NAME, "fiftypm_api").
-endif.

-define(K8S_DNS_PRE, "svr@" ++ ?K8S_SVC_NAME ++ "-").
-define(K8S_DNS_POST, "." ++ ?K8S_SVC_NAME ++ ".default.svc.cluster.local").

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = init(),
    fiftypm_api_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

init() ->
    Np = connect_prev_node(node()),
    {atomic, ok} = start_mnesia_init_tab(Np),
    {ok, _} = start_cowboy(state0()).

%% Establish Mnesia db cluster as the each Erlang nodes boots as a pod of 
%% one StatefulSet. Each node's name has a predictable name in a format 
%% "server@servicename-X.servicename.default.svc.cluster.local", where the
%% name and X is assigned by a Kubernetes StatefulSet. When node "X" starts,
%% it tries to connect to node X - 1 to join the Erlang cluster.
%% TODO: the following code only works for X, where 0 <= X <= 9
connect_prev_node(N) when is_atom(N) -> connect_prev_node(atom_to_list(N));
connect_prev_node("nonode@nohost") -> none;
connect_prev_node(?K8S_DNS_PRE ++ [N | _]) -> connect_prev_node(N);
connect_prev_node($0) -> none;
connect_prev_node(N) when is_integer(N) ->    
    Np0 = [?K8S_DNS_PRE, N-1, ?K8S_DNS_POST],
    Np1 = list_to_atom(lists:flatten(Np0)),
    pong = net_adm:ping(Np1),
    Np1.

%% On the current node, initialize cluster schema and ensure it is of storage 
%% type disc_copies and offer server application the chance to initialize table 
%% or copy table from initialized nodes before this node
%% On first node or the development nonode@nohost
%% - If creates a RAM schema first, then start Mnesia
%% - If schema creation failed, which implies there was a schema, first node
%%   initialization is done
%% - If schema creation succeeded, which implies this is a brand new Mnesia 
%%   node, proceed to initialize other tables
%% On node X of all subsequent nodes
%% - Start mnesia, it will also succeed
%% - Join this node to the Mnesia cluster, if not joined yet, by adding
%%   X - 1 node as extra db node
%%   - If schema type is ram_copies, it may or may not have joined cluster yet
%%     regardless, we add the X - 1 node as one extra mnesia db node. Adding
%%     X - 1 to cluster does not change schema storage type.
%%   - If schema type is disc_copies, it must have joned the cluster, thus 
%%     proceed to next step
%% - Now change schema to disc_copies
%%   - If schema is still ram_copies, change its type to disc_copies, then
%%     copy tables from the rest of the Mnesia cluster to this node (X). 
start_mnesia_init_tab(none) -> 
    CreateSchemaRet = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    init_tab_0(CreateSchemaRet);
start_mnesia_init_tab(Np) when is_atom(Np) ->
    ok = mnesia:start(),
    SchemaStorageType0 = mnesia:table_info(schema, storage_type),
    add_this_node(SchemaStorageType0, Np),
    SchemaStorageType1 = mnesia:table_info(schema, storage_type),
    init_tab_n(SchemaStorageType1).

init_tab_0(ok) -> 
    {atomic, ok} = tab_session:create_table(),
    {atomic, ok} = tab_kv:create_table();
init_tab_0({error, {_,{already_exists, _}}}) -> {atomic, ok}.

add_this_node(ram_copies, Np) -> {ok, _} = mnesia:change_config(extra_db_nodes, [Np]);
add_this_node(disc_copies, _) -> {ok, ignored}.

init_tab_n(ram_copies) -> 
    {atomic, ok} = mnesia:change_table_copy_type(schema, node(), disc_copies),
    {atomic, ok} = tab_session:add_table_copy(),
    {atomic, ok} = tab_kv:add_table_copy();
init_tab_n(disc_copies) -> {atomic, ok}.

%%
start_cowboy(S) ->
    Dispatch = cowboy_router:compile(routes(S)),
    {ok, _} = cowboy:start_clear(fiftypm_api_listner, [{port, 80}], #{env => #{dispatch => Dispatch}}).

routes(S) -> [route0(S)].
route0(S) -> {'_', [{prefix("/v1/probes/:pb"), fiftypm_api_probes, S},
                    {prefix("/v1/session"), fiftypm_api_session, S},
                    {prefix("/login/[...]"), fiftypm_api_login, S},
                    {'_', fiftypm_api_badreq, []}]}.                

prefix(Path) -> application:get_env(fiftypm_api, prefix, "") ++ Path.

state0() -> #{}.
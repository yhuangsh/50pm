-module(tab_kv).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export(
    [
        create_table/0,
        add_table_copy/0,

        dset/1, dset/2,
        dget/1, 
        ddel/1
    ]
).

-define(TAB_KV, kv).

%%====================================================================
%% API
%%====================================================================

%% Bootstrap
create_table() -> 
    mnesia:create_table(?TAB_KV, [{ram_copies, [node()]}]).
add_table_copy() ->
    mnesia:add_table_copy(?TAB_KV, node(), ram_copies).

%% CRUD - Dirty version
dset({K, V}) -> dset(K, V).
dset(K, V) -> mnesia:dirty_write({?TAB_KV, K, V}).
dget(K) -> dget_1(mnesia:dirty_read(?TAB_KV, K)).
dget_1([]) -> undefined;
dget_1([{?TAB_KV, K, V}]) -> {K, V}.
ddel(K) -> mnesia:dirty_delete(?TAB_KV, K).

%%====================================================================
%% Unit tests
%%====================================================================

-ifdef(TEST).

crud_test_() ->
    {
        setup, 
        fun setup/0, 
        fun cleanup/1, 
        fun (D) ->
            [
                test_dset(D),
                test_dget(D),
                test_ddel(D)
            ] 
        end
    }.

setup() ->
    mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    {atomic, ok} = create_table(),
    S0 = {k0, v0},
    Ret = dset(S0),
    S0.

cleanup(_) -> 
    ?assertEqual({atomic, ok}, mnesia:delete_table(?TAB_KV)),
    ?assertEqual(stopped, mnesia:stop()).

test_dset(_S0) ->    
    V1 = #{ d1 => <<"data1">> },
    S1 = {k1, V1},
    ok = dset(S1),
    [
        ?_assertMatch(S1, dget(k1))
    ].

test_dget(S0 = {k0, v0}) ->
    [
        ?_assertMatch(S0, dget(k0)),
        ?_assertEqual(undefined, dget(not_exist))
    ].

test_ddel(S0 = {k0, v0}) ->
    [
        ?_assertEqual(ok, ddel(k0)),
        ?_assertEqual(undefined, dget(S0))
    ].

-endif.
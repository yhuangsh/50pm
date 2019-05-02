-module(tab_kv).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export(
    [
        create_table/1,
        add_table_copy/1,

        dset/2, dset/3,
        dget/2, 
        ddel/2
    ]
).

%%====================================================================
%% API
%%====================================================================

%% Bootstrap
create_table([Tab | RestTabs])  -> create_table(Tab), create_table(RestTabs);
create_table(TabName) when is_atom(TabName) -> create_table({TabName, ram});
create_table({TabName, ram}) when is_atom(TabName) -> {atomic, ok} = mnesia:create_table(TabName, [{ram_copies, [node()]}]);
create_table({TabName, disc}) when is_atom(TabName) -> {atomic, ok} = mnesia:create_table(TabName, [{disc_copies, [node()]}]). 

add_table_copy([Tab | RestTabs])  -> add_table_copy(Tab), add_table_copy(RestTabs);
add_table_copy(TabName) when is_atom(TabName) -> add_table_copy({TabName, ram});
add_table_copy({TabName, ram}) when is_atom(TabName) -> {atomic, ok} = mnesia:add_table_copy(TabName, node(), ram_copies);
add_table_copy({TabName, disc}) when is_atom(TabName) -> {atomic, ok} = mnesia:add_table_copy(TabName, node(), disc_copies). 

%% CRUD - Dirty
dset(T, {K, V}) -> dset(T, K, V).
dset(T, K, V) -> mnesia:dirty_write({T, K, V}).
dget(T, K) -> dget_1(mnesia:dirty_read(T, K)).
dget_1([]) -> undefined;
dget_1([{_T, K, V}]) -> {K, V}.
ddel(T, K) -> mnesia:dirty_delete(T, K).

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
    {atomic, ok} = create_table(kv_test),
    S0 = {k0, v0},
    Ret = dset(kv_test, S0),
    S0.

cleanup(_) -> 
    ?assertEqual({atomic, ok}, mnesia:delete_table(kv_test)),
    ?assertEqual(stopped, mnesia:stop()).

test_dset(_S0) ->    
    V1 = #{ d1 => <<"data1">> },
    S1 = {k1, V1},
    ok = dset(kv_test, S1),
    [
        ?_assertMatch(S1, dget(kv_test, k1))
    ].

test_dget(S0 = {k0, v0}) ->
    [
        ?_assertMatch(S0, dget(kv_test, k0)),
        ?_assertEqual(undefined, dget(kv_test, not_exist))
    ].

test_ddel(S0 = {k0, v0}) ->
    [
        ?_assertEqual(ok, ddel(kv_test, k0)),
        ?_assertEqual(undefined, dget(kv_test, S0))
    ].

-endif.
-module(tab_session).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export(
    [
        create_table/0,
        add_table_copy/0,

        create/0, create/1,
        read/1, 
        update/2,
        delete/1
    ]
).

-record(session, {key, val}).

-define(TAB_SESSION, session).


%%====================================================================
%% API
%%====================================================================

%% Bootstrap
create_table() -> 
    mnesia:create_table(?TAB_SESSION, [{disc_copies, [node()]}]).
add_table_copy() ->
    mnesia:add_table_copy(?TAB_SESSION, node(), disc_copies).

%% CRUD
create() -> create(#{}).
create(Val) when is_map(Val) -> 
    Sid = mk_sid(),
    mnesia:dirty_write(#session{key = Sid, val = Val}), 
    Sid.
read(Sid) when is_binary(Sid) -> mnesia:dirty_read(?TAB_SESSION, Sid).
update(Sid, Val) when is_binary(Sid), is_map(Val) -> mnesia:dirty_write(#session{key = Sid, val = Val}).
delete(Sid) when is_binary(Sid) -> mnesia:dirty_delete(?TAB_SESSION, Sid).

mk_sid() -> base64:encode(crypto:strong_rand_bytes(20)).

%%====================================================================
%% Unit tests
%%====================================================================

-ifdef(TEST).

-define(SID_LEN, 28).

crud_test_() ->
    {
        setup, 
        fun setup/0, 
        fun cleanup/1, 
        fun (D) ->
            [
                test_create(D),
                test_read(D),
                test_update(D),
                test_delete(D)
            ] 
        end
    }.

setup() ->
    mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    {atomic, ok} = create_table(),
    _S0 = create().

cleanup(_) -> 
    ?assertEqual({atomic, ok}, mnesia:delete_table(?TAB_SESSION)),
    ?assertEqual(stopped, mnesia:stop()).

test_create(_S0) ->    
    V1 = #{ d1 => <<"data1">> },
    S1 = create(V1),
    [
        ?_assertEqual(?SID_LEN, size(S1)),
        ?_assertMatch([{session, S1, V1}], read(S1))
    ].

test_read(S0) ->
    [
        ?_assertMatch([{session, S0, #{}}], read(S0)),
        ?_assertEqual([], read(<<"non-exist">>))
    ].

test_update(S0) ->
    V0p = #{ d2 => <<"S0 prime">> },
    [   
        ?_assertEqual(ok, update(S0, V0p)),
        ?_assertEqual([{session, S0, V0p}], read(S0))
    ].

test_delete(S0) ->
    [
        ?_assertEqual(ok, delete(S0)),
        ?_assertEqual([], read(S0))
    ].

-endif.
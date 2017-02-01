-module(jsxd_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================


new_test() ->
    ?assertEqual([], jsxd:new()).


from_list_test() ->
    ?assertEqual([2,1], jsxd:from_list([2,1])),
    ?assertEqual(#{<<"a">> => 2, <<"b">> => 1}, jsxd:from_list([{<<"b">>, 1}, {<<"a">>, 2}])).

default_test() ->
    V = #{<<"a">> => 1},
    ?assertEqual(V, jsxd:default(<<"a">>, 2, V)),
    ?assertEqual(#{<<"a">> => 1, <<"b">> => 2}, jsxd:default(<<"b">>, 2, V)).

get_arr_test() ->
    SubArr = [10,20,30,40,50,60],
    Arr = jsxd:from_list([1,SubArr,3,4,5,6]),
    ?assertEqual({ok, 1},
                 jsxd:get(0, Arr)),
    ?assertEqual({ok, SubArr},
                 jsxd:get(1, Arr)),
    ?assertEqual({ok, 6},
                 jsxd:get(5, Arr)),
    ?assertEqual(undefined,
                 jsxd:get(6, Arr)),
    ?assertEqual({ok, 10},
                 jsxd:get([1,0], Arr)),
    ?assertEqual({ok, 60},
                 jsxd:get([1,5], Arr)),
    ?assertEqual(undefined,
                 jsxd:get([2,5], Arr)),
    ?assertEqual(42,
                 jsxd:get([2,5], 42, Arr)).


get_obj_test() ->
    SubArr = [10,20,30,40,50,60],
    SubObj = [{<<"a">>, 11}, {<<"b">>, 12}],
    Obj = jsxd:from_list([{<<"b">>, SubArr}, {<<"obj">>, SubObj}, {<<"int">>, 1}]),
    ?assertEqual({ok, 1},
                 jsxd:get(<<"int">>, Obj)),
    ?assertEqual(undefined,
                 jsxd:get(<<"float">>, Obj)),
    ?assertEqual(undefined,
                 jsxd:get(1, Obj)),
    ?assertEqual(42,
                 jsxd:get(1, 42, Obj)),
    ?assertEqual({ok, 20},
                 jsxd:get([<<"b">>, 1], Obj)),
    ?assertEqual({ok, 11},
                 jsxd:get([<<"obj">>, <<"a">>], Obj)),
    ?assertEqual({ok, 20},
                 jsxd:get(<<"b[1]">>, Obj)),
    ?assertEqual({ok, 11},
                 jsxd:get(<<"obj.a">>, Obj)).

select_test() ->
    Obj = jsxd:from_list([{<<"b">>, 1}, {<<"obj">>, 2}, {<<"int">>, 3}]),
    ?assertEqual(#{<<"obj">> => 2},
                 jsxd:select([<<"obj">>], Obj)),
    ?assertEqual(#{<<"b">> => 1},
                 jsxd:select([<<"b">>], Obj)),
    ?assertEqual(#{<<"b">> => 1, <<"obj">> => 2},
                 jsxd:select([<<"b">>, <<"obj">>], Obj)).

set_arr_test() ->
    SubArr = [10,20,30],
    Arr = jsxd:from_list([1,SubArr,3]),
    ?assertEqual([99, SubArr, 3],
                 jsxd:set(0, 99, Arr)),
    ?assertEqual([1, SubArr, 99],
                 jsxd:set(2, 99, Arr)),
    ?assertEqual([1, SubArr, 3, 99],
                 jsxd:set(3, 99, Arr)),
    ?assertEqual([1, SubArr, 3, null, 99],
                 jsxd:set(4, 99, Arr)),
    ?assertEqual([1, [99, 20, 30], 3],
                 jsxd:set([1,0], 99, Arr)),
    ok.

set_obj_test() ->
    SubArr = [10,20,30],
    SubObj = [{<<"a">>, 11}, {<<"b">>, 12}],
    SubMap = jsxd:from_list(SubObj),
    Obj = jsxd:from_list([{<<"b">>, SubArr}, {<<"obj">>, SubObj}, {<<"int">>, 1}]),

    ?assertEqual(#{<<"b">> => SubArr, <<"int">> => 2, <<"obj">> => SubMap},
                 jsxd:set(<<"int">>, 2, Obj)),

    ?assertEqual(#{<<"b">> => [10,99,30], <<"int">> => 1, <<"obj">> => SubMap},
                 jsxd:set([<<"b">>, 1], 99, Obj)),

    ?assertEqual(#{<<"b">> => SubArr, <<"int">> => 1,
                   <<"obj">> => #{<<"a">> => 99, <<"b">> => 12}},
                 jsxd:set([<<"obj">>, <<"a">>], 99, Obj)),

    ?assertEqual(#{<<"x">> => 1},
                 jsxd:set(<<"x">>, 1, [])),

    ?assertEqual(#{<<"x">> => #{<<"y">> => 1}},
                 jsxd:set([<<"x">>, <<"y">>], 1, [])),

    ?assertEqual(#{<<"b">> => SubArr, <<"int">> => 1,
                   <<"obj">> => SubMap#{<<"c">> => 99}},
                 jsxd:set([<<"obj">>, <<"c">>], 99, Obj)),

    ?assertEqual(#{<<"b">> => SubArr, <<"int">> => 1,
                   <<"obj">> => SubMap#{<<"c">> => #{<<"c">> => 99}}},
                 jsxd:set([<<"obj">>, <<"c">>, <<"c">>], 99, Obj)),

    ok.

update_arr_test() ->
    SubArr = [10,20,30],
    Arr = jsxd:from_list([1,SubArr,3]),
    Inc = fun(X) -> X + 1 end,
    ?assertEqual([2, SubArr, 3],
                 jsxd:update(0, Inc, Arr)),
    ?assertEqual([1, [10, 21, 30], 3],
                 jsxd:update([1,1], Inc, Arr)),
    ?assertEqual([2, SubArr, 3],
                 jsxd:update(0, Inc, Arr)),
    ?assertEqual(Arr,
                 jsxd:update([3,1], Inc, Arr)),
    ?assertEqual(Arr ++ [0],
                 jsxd:update(3, Inc, 0, Arr)),
    ?assertEqual(Arr ++ [[null, 0]],
                 jsxd:update([3, 1], Inc, 0, Arr)),
    ok.

update_obj_test() ->
    SubArr = [10,20,30],
    SubObj = [{<<"a">>, 11}, {<<"b">>, 12}],
    SubMap = jsxd:from_list(SubObj),
    Obj = jsxd:from_list([{<<"b">>, SubArr}, {<<"obj">>, SubObj}, {<<"int">>, 1}]),
    Inc = fun(X) -> X + 1 end,
    ?assertEqual(#{<<"b">> => SubArr, <<"int">> => 2, <<"obj">> => SubMap},
                 jsxd:update(<<"int">>, Inc , Obj)),
    ?assertEqual(#{<<"b">> => [10,21,30], <<"int">> => 1, <<"obj">> => SubMap},
                 jsxd:update([<<"b">>, 1], Inc, Obj)),
    ?assertEqual(Obj,
                 jsxd:update([<<"x">>, 1], Inc, Obj)),
    ?assertEqual(Obj#{<<"x">> => 0},
                 jsxd:update([<<"x">>], Inc, 0, Obj)),
    ?assertEqual(Obj#{<<"x">> => #{<<"y">> => 0}},
                 jsxd:update([<<"x">>, <<"y">>], Inc, 0, Obj)),

    ok.

delete_arr_test() ->
    SubArr = [10,20,30],
    Arr = jsxd:from_list([1,SubArr,3]),
    ?assertEqual([SubArr, 3],
                 jsxd:delete(0, Arr)),
    ok.

delete_obj_test() ->
    SubArr = [10,20,30],
    SubObj = [{<<"a">>, 11}, {<<"b">>, 12}],
    Obj = jsxd:from_list([{<<"b">>, SubArr}, {<<"obj">>, SubObj}, {<<"int">>, 1}]),
    ?assertEqual(#{<<"b">> => SubArr, <<"obj">> => jsxd:from_list(SubObj)},
                 jsxd:delete(<<"int">> , Obj)),
    ok.
map_arr_test() ->
    Arr = jsxd:from_list([1,3,6]),
    ?assertEqual([2*6, 1*3, 0*1],
                 jsxd:map(fun(Idx,Val) ->
                                  Idx*Val
                          end, Arr)),
    ok.

map_obj_test() ->
    Obj = [{<<"a">>,1},{<<"b">>, 2},{<<"c">>, 3}],
    ?assertEqual([{<<"a">>, 1}, {<<"b">>, 4}, {<<"c">>, 9}],
                 jsxd:map(fun(_K,Val) ->
                                  Val*Val
                          end, Obj)).

map_map_test() ->
    Obj = jsxd:from_list([{<<"a">>,1},{<<"b">>, 2},{<<"c">>, 3}]),
    ?assertEqual(#{<<"a">> => 1, <<"b">> => 4, <<"c">> => 9},
                 jsxd:map(fun(_K,Val) ->
                                  Val*Val
                          end, Obj)).

fold_arr_test() ->
    Arr = [1,3,6],
    ?assertEqual(0*1+1*3+2*6,
                 jsxd:fold(fun(Idx, Val, Acc) ->
                                   (Idx*Val) + Acc
                           end, 0, Arr)),
    ok.

fold_obj_test() ->
    Obj = jsxd:from_list([{<<"a">>,1},{<<"b">>, 2},{<<"c">>, 3}]),
    ?assertEqual(1+2+3,
                 jsxd:fold(fun(_K, Val, Acc) ->
                                   Val + Acc
                           end, 0, Obj)),
    ok.

merge_test() ->
    Obj1 = jsxd:from_list([{<<"a">>, 1}, {<<"b">>, 2}, {<<"d">>, 4},
                           {<<"f">>, 6}]),
    Obj2 = jsxd:from_list([{<<"b">>, 99}, {<<"c">>, 3}, {<<"e">>, 5}]),
    ?assertEqual(#{<<"a">> => 1, <<"b">> => 2, <<"c">> => 3, <<"d">> => 4,
                   <<"e">> => 5, <<"f">> => 6},
                 jsxd:merge(Obj1, Obj2)).

merge_conf_test() ->
    Obj1 = jsxd:from_list([{<<"b">>, 1}, {<<"c">>, 3}, {<<"e">>, 5}]),
    Obj2 = jsxd:from_list([{<<"a">>, 1}, {<<"b">>, 1}, {<<"d">>, 4},
                           {<<"f">>, 6}]),
    ?assertEqual(#{<<"a">> => 1, <<"b">> => 2, <<"c">> => 3, <<"d">> => 4,
                   <<"e">> => 5, <<"f">> => 6},
                 jsxd:merge(fun(_, A, B) -> A + B end, Obj1, Obj2)).

array_set_test() ->
    Exp =[#{<<"ip">> => <<"127.0.0.1">>,
            <<"nic_tag">> => <<"admin">>,
            <<"primary">> => true}],
    O = [#{<<"ip">> => <<"127.0.0.1">>,<<"nic_tag">> => <<"admin">>}],
    Res = jsxd:set([0, <<"primary">>],true ,O),
    ?assertEqual(Exp, Res).

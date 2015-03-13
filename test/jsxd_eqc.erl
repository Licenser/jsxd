-module(jsxd).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").

-type key()::binary()|integer().

-type keys()::key()|[key()].

-type value()::binary()|number()|object()|jsxarray()|null|true|false.

-type object()::[{binary(), value()}].

-type jsxarray()::[value()] | [{}].


%%%===================================================================
%%% Tests
%%%===================================================================


new_test() ->
    ?assertEqual([], jsxd:new()).


from_list_test() ->
    ?assertEqual([2,1], jsxd:from_list([2,1])),
    ?assertEqual([{<<"a">>, 2}, {<<"b">>, 1}], jsxd:from_list([{<<"b">>, 1}, {<<"a">>, 2}])),
    ?assertEqual([[{<<"a">>, 2}, {<<"b">>, 1}], 2], jsxd:from_list([[{<<"b">>, 1}, {<<"a">>, 2}], 2])).

get_arr_test() ->
    SubArr = [10,20,30,40,50,60],
    Arr = from_list([1,SubArr,3,4,5,6]),
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
    Obj = from_list([{<<"b">>, SubArr}, {<<"obj">>, SubObj}, {<<"int">>, 1}]),
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
    Obj = from_list([{<<"b">>, 1}, {<<"obj">>, 2}, {<<"int">>, 3}]),
    ?assertEqual([{<<"obj">>, 2}],
                 jsxd:select([<<"obj">>], Obj)),
    ?assertEqual([{<<"b">>, 1}],
                 jsxd:select([<<"b">>], Obj)),
    ?assertEqual([{<<"b">>, 1}, {<<"obj">>, 2}],
                 jsxd:select([<<"b">>, <<"obj">>], Obj)).

set_arr_test() ->
    SubArr = [10,20,30],
    Arr = from_list([1,SubArr,3]),
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
    Obj = from_list([{<<"b">>, SubArr}, {<<"obj">>, SubObj}, {<<"int">>, 1}]),

    ?assertEqual([{<<"b">>, SubArr}, {<<"int">>, 2}, {<<"obj">>, SubObj}],
                 jsxd:set(<<"int">>, 2, Obj)),

    ?assertEqual([{<<"b">>, [10,99,30]}, {<<"int">>, 1}, {<<"obj">>, SubObj}],
                 jsxd:set([<<"b">>, 1], 99, Obj)),

    ?assertEqual([{<<"b">>, SubArr}, {<<"int">>, 1}, {<<"obj">>, [{<<"a">>, 99}, {<<"b">>, 12}]}],
                 jsxd:set([<<"obj">>, <<"a">>], 99, Obj)),

    ?assertEqual([{<<"x">>, 1}],
                 jsxd:set(<<"x">>, 1, [])),

    ?assertEqual([{<<"x">>, [{<<"y">>, 1}]}],
                 jsxd:set([<<"x">>, <<"y">>], 1, [])),

    ?assertEqual([{<<"b">>, SubArr}, {<<"int">>, 1}, {<<"obj">>, [{<<"a">>, 11}, {<<"b">>, 12}, {<<"c">>, 99}]}],
                 jsxd:set([<<"obj">>, <<"c">>], 99, Obj)),

    ?assertEqual([{<<"b">>, SubArr}, {<<"int">>, 1}, {<<"obj">>, [{<<"a">>, 11}, {<<"b">>, 12}, {<<"c">>, [{<<"c">>, 99}]}]}],
                 jsxd:set([<<"obj">>, <<"c">>, <<"c">>], 99, Obj)),

    ok.

update_arr_test() ->
    SubArr = [10,20,30],
    Arr = from_list([1,SubArr,3]),
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
    Obj = from_list([{<<"b">>, SubArr}, {<<"obj">>, SubObj}, {<<"int">>, 1}]),
    Inc = fun(X) -> X + 1 end,
    ?assertEqual([{<<"b">>, SubArr}, {<<"int">>, 2}, {<<"obj">>, SubObj}],
                 jsxd:update(<<"int">>, Inc , Obj)),
    ?assertEqual([{<<"b">>, [10,21,30]}, {<<"int">>, 1}, {<<"obj">>, SubObj}],
                 jsxd:update([<<"b">>, 1], Inc , Obj)),
    ?assertEqual(Obj,
                 jsxd:update([<<"x">>, 1], Inc , Obj)),
    ?assertEqual(Obj  ++ [{<<"x">>, 0}],
                 jsxd:update([<<"x">>], Inc, 0, Obj)),
    ?assertEqual(Obj  ++ [{<<"x">>, [{<<"y">>, 0}]}],
                 jsxd:update([<<"x">>, <<"y">>], Inc, 0, Obj)),

    ok.

delete_arr_test() ->
    SubArr = [10,20,30],
    Arr = from_list([1,SubArr,3]),
    ?assertEqual([SubArr, 3],
                 jsxd:delete(0, Arr)),
    ok.

delete_obj_test() ->
    SubArr = [10,20,30],
    SubObj = [{<<"a">>, 11}, {<<"b">>, 12}],
    Obj = from_list([{<<"b">>, SubArr}, {<<"obj">>, SubObj}, {<<"int">>, 1}]),
    ?assertEqual([{<<"b">>, SubArr}, {<<"obj">>, SubObj}],
                 jsxd:delete(<<"int">> , Obj)),
    ok.
map_arr_test() ->
    Arr = from_list([1,3,6]),
    ?assertEqual([2*6, 1*3, 0*1],
                 jsxd:map(fun(Idx,Val) ->
                                  Idx*Val
                          end, Arr)),
    ok.

map_obj_test() ->
    Obj = [{<<"a">>,1},{<<"b">>, 2},{<<"c">>, 3}],
    ?assertEqual([{<<"a">>,1},{<<"b">>, 4},{<<"c">>, 9}],
                 jsxd:map(fun(_K,Val) ->
                                  Val*Val
                          end, Obj)),
    ok.

fold_arr_test() ->
    Arr = [1,3,6],
    ?assertEqual(0*1+1*3+2*6,
                 jsxd:fold(fun(Idx, Val, Acc) ->
                                     (Idx*Val) + Acc
                             end, 0, Arr)),
    ok.

fold_obj_test() ->
    Obj = from_list([{<<"a">>,1},{<<"b">>, 2},{<<"c">>, 3}]),
    ?assertEqual(1+2+3,
                 jsxd:fold(fun(_K, Val, Acc) ->
                                     Val + Acc
                             end, 0, Obj)),
    ok.

merge_test() ->
    Obj1 = from_list([{<<"a">>, 1}, {<<"b">>, 2}, {<<"d">>, 4}, {<<"f">>, 6}]),
    Obj2 = from_list([{<<"b">>, 99}, {<<"c">>, 3}, {<<"e">>, 5}]),
    ?assertEqual([{<<"a">>, 1}, {<<"b">>, 2}, {<<"c">>, 3}, {<<"d">>, 4}, {<<"e">>, 5}, {<<"f">>, 6}],
                 jsxd:merge(Obj1, Obj2)).

merge_conf_test() ->
    Obj1 = from_list([{<<"b">>, 1}, {<<"c">>, 3}, {<<"e">>, 5}]),
    Obj2 = from_list([{<<"a">>, 1}, {<<"b">>, 1}, {<<"d">>, 4}, {<<"f">>, 6}]),
    ?assertEqual([{<<"a">>, 1}, {<<"b">>, 2}, {<<"c">>, 3}, {<<"d">>, 4}, {<<"e">>, 5}, {<<"f">>, 6}],
                 jsxd:merge(fun(_, A, B) -> A + B end, Obj1, Obj2)).

key() ->
    ?LET(Cs,
         non_empty(
           list(oneof(
                  [$a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m,
                   $n, $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
                   $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M,
                   $N, $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z]))),
         list_to_binary(Cs)).

keys() ->
    non_empty(list(key())).


value() ->
    ?SIZED(Size, value(Size)).

value(0) ->
    oneof([binary(), number(), null, true, false]);

value(S) ->
    ?LAZY(weighted_union(
            [{1, object(S-1)},
             {30, value(0)}])).

array() ->
    list(value()).

object() ->
    ?SIZED(Size, object(Size)).

object(N) ->
    list({key(), value(N)}).

prop_delete() ->
    ?FORALL({K,O}, {keys(), object()},
            jsxd:get(K, delete(K, O)) =:= undefined).

prop_set_get() ->
    ?FORALL({K, V, O}, {keys(), value(), object()},
            jsxd:get(K, jsxd:set(K, V, O)) =:= {ok, V}).

prop_append() ->
    ?FORALL({K, V, L, O},
            {keys(), value(), array(), object()},
            begin
                O1 = jsxd:set(K, L, O),
                O2 = jsxd:append(K, V, O1),
                jsxd:get(K, O2) =:= {ok, L ++ [V]}
            end).

prop_prepend() ->
    ?FORALL({K, V, L, O},
            {keys(), value(), array(), object()},
            begin
                O1 = jsxd:set(K, L, O),
                O2 = jsxd:prepend(K, V, O1),
                jsxd:get(K, O2) =:= {ok, [V | L]}
            end).

proper_test_() ->
    {timeout, 60, ?_assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result]))}.

proper_test() ->
    ?assertEqual(true, proper:check_spec({jsxd, new, 0}, [{to_file, user}, long_result])),
    %% ?assertEqual(true, proper:check_spec({jsxd, from_list, 1}, [{to_file, user}, long_result])),
    %% ?assertEqual(true, proper:check_spec({jsxd, select, 2}, [{to_file, user}, long_result])),
    %% ?assertEqual(true, proper:check_spec({jsxd, set, 3}, [{to_file, user}, long_result])),
    %% ?assertEqual(true, proper:check_spec({jsxd, delete, 2}, [{to_file, user}, long_result])),
    %% ?assertEqual(true, proper:check_spec({jsxd, update, 3}, [{to_file, user}, long_result])),
    %% ?assertEqual(true, proper:check_spec({jsxd, update, 4}, [{to_file, user}, long_result])),
    %% ?assertEqual(true, proper:check_spec({jsxd, map, 2}, [{to_file, user}, long_result])),
    %% ?assertEqual(true, proper:check_spec({jsxd, fold, 3}, [{to_file, user}, long_result])),
    ?assertEqual(true, proper:check_spec({jsxd, merge, 2}, [{to_file, user}, long_result])),
    %% ?assertEqual(true, proper:check_spec({jsxd, merge, 3}, [{to_file, user}, long_result])),
    %% ?assertEqual(true, proper:check_spec({jsxd, thread, 2}, [{to_file, user}, long_result])),
    ok.

-endif.
-endif.

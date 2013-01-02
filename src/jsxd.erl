-module(jsxd).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0,
         get/2,
         set/3,
         delete/2,
         update/3,
         update/4,
         map/2,
         reduce/3]).

-type key()::binary()|integer().

-type keys()::key()|[key()].

-type value()::binary()|number()|object()|jsxarray()|null.

-type object()::[{key(), value()}].

-type jsxarray()::[value()].

-spec new() -> jsxarray() | object().

new() ->
    [].


-spec get(Key::keys(), Obj::object()|jsxarray()) -> value().

get(Key, Obj) when is_list(Obj),
                   (is_integer(Key) orelse
                    is_binary(Key)) ->
    get([Key], Obj);

get([], Obj) ->
    Obj;

get([Pos], [H | _T] = Arr) when is_integer(Pos),
                                (is_number(H) orelse
                                 is_binary(H) orelse
                                 is_list(H) orelse
                                 is_atom(H))->
    try lists:nth(Pos + 1, Arr) of
        Value ->
            {ok, Value}
    catch
        _:_ ->
            not_found
    end;

get([Key], [{_, _} | _T] = Obj) when is_binary(Key) ->
    case lists:keyfind(Key, 1, Obj) of
        {Key, Value} ->
            {ok, Value};
        _ ->
            not_found
    end;

%% This faults when the key has a wrong format.
get([_Key], _Obj) ->
    not_found;

get([Key | Keys], Obj) when is_list(Obj),
                            (is_binary(Key) orelse
                             is_integer(Key)) ->
    case jsxd:get([Key], Obj) of
        {ok, Obj1} when is_list(Obj1) ->
            jsxd:get(Keys, Obj1);
        _ ->
            not_found
    end.

set(Key, Val, Obj) when not is_list(Key) ->
    set([Key], Val, Obj);



set([Pos], Val, [H | _T] = Arr) when is_integer(Pos),
                                     (is_number(H) orelse
                                      is_binary(H) orelse
                                      is_list(H) orelse
                                      is_atom(H)) ->
    set_arr(Pos, Val, Arr);

set([Pos], Val, [] = Arr) when is_integer(Pos) ->
    set_arr(Pos, Val, Arr);

set([Key], Val, [{_, _} | _T] = Obj) when is_binary(Key) ->
    lists:keystore(Key, 1, Obj, {Key, Val});

set([Key], Val, [] = Obj) when is_binary(Key) ->
    lists:keystore(Key, 1, Obj, {Key, Val});

set([Key | [_ | _] = Keys], Value, Obj) ->
    Obj1 = case jsxd:get([Key], Obj) of
               not_found ->
                   jsxd:set(Keys, Value, jsxd:new());
               {ok, SubObj1} ->
                   jsxd:set(Keys, Value, SubObj1)
           end,
    jsxd:set([Key], Obj1, Obj).

delete(Key, Obj) when not is_list(Key)->
    delete([Key], Obj);


delete([Key], [{_, _} | _T] = Obj) when is_binary(Key) ->
    lists:keydelete(Key, 1, Obj);

delete([Pos], [H | _T] = Arr) when is_integer(Pos),
                                     (is_number(H) orelse
                                      is_binary(H) orelse
                                      is_list(H) orelse
                                      is_atom(H))->
    try lists:nthtail(Pos + 1, Arr) of
        T ->
            lists:sublist(Arr, Pos) ++ T
    catch
        _:_ ->
            lists:sublist(Arr, Pos)
    end;

delete([_], Obj) ->
    Obj;

delete(Keys, Obj) ->
    LastKey = lists:last(Keys),
    ButLast = lists:sublist(Keys, length(Keys) - 1),
    jsxd:update(ButLast, fun(Obj1) ->
                                 jsxd:delete([LastKey], Obj1)
                         end, Obj).

update(Keys, UpdateFn, Obj) ->
    case jsxd:get(Keys, Obj) of
        not_found ->
            Obj;
        {ok, Val} ->
            jsxd:set(Keys, UpdateFn(Val), Obj)
    end.

update(Keys, UpdateFn, Default, Obj) ->
    case jsxd:get(Keys, Obj) of
        not_found ->
            jsxd:set(Keys, Default, Obj);
        {ok, Val} ->
            jsxd:set(Keys, UpdateFn(Val), Obj)
    end.


map(MapFn, [{_, _} | _] = Obj) ->
    lists:map(fun ({K, V}) ->
                      V1 = MapFn(K, V),
                      {K, V1}
              end, Obj);

map(MapFn, Obj) ->
    {_, Res} = lists:foldl(fun (Elem, {I, ObjAcc}) ->
                                   {I + 1, [MapFn(I, Elem) | ObjAcc]}
                           end, {0, []}, Obj),
    Res.



reduce(ReduceFn, Acc0, [{_, _} | _] = Obj) ->
    lists:foldl(fun ({K, V}, Acc) ->
                                   ReduceFn(K, V, Acc)
                           end, Acc0, Obj);

reduce(ReduceFn, Acc0, Obj) ->
    {_, Res} = lists:foldl(fun (Elem, {I, Acc}) ->
                                   {I + 1, ReduceFn(I, Elem, Acc)}
                           end, {0, Acc0}, Obj),
    Res.

%%%===================================================================
%%% Internal functions
%%%===================================================================

pad_list(N, Val) when N =< 0->
    [Val];

pad_list(N, Val) ->
    [null | pad_list(N-1, Val)].

set_arr(Pos, Val, Arr) ->
    Tail = try lists:nthtail(Pos + 1, Arr) of
               T ->
                   T
           catch
               _:_ ->
                   []
           end,
    Head = lists:sublist(Arr, Pos),
    HeadLen = length(Head),
    Tail1 = if
                HeadLen < Pos ->
                    pad_list(Pos - HeadLen, Val);
                true ->
                    [Val | Tail]
            end,
    Head ++ Tail1.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

new_test() ->
    ?assertEqual(jsxd:new(), []).

get_arr_test() ->
    SubArr = [10,20,30,40,50,60],
    Arr = [1,SubArr,3,4,5,6],
    ?assertEqual({ok, 1},
                 jsxd:get(0, Arr)),
    ?assertEqual({ok, SubArr},
                 jsxd:get(1, Arr)),
    ?assertEqual({ok, 6},
                 jsxd:get(5, Arr)),
    ?assertEqual(not_found,
                 jsxd:get(6, Arr)),
    ?assertEqual({ok, 10},
                 jsxd:get([1,0], Arr)),
    ?assertEqual({ok, 60},
                 jsxd:get([1,5], Arr)),
    ?assertEqual(not_found,
                 jsxd:get([2,5], Arr)).

get_obj_test() ->
    SubArr = [10,20,30,40,50,60],
    SubObj = [{<<"a">>, 11}, {<<"b">>, 12}],
    Obj = [{<<"b">>, SubArr}, {<<"obj">>, SubObj}, {<<"int">>, 1}],
    ?assertEqual({ok, 1},
                 jsxd:get(<<"int">>, Obj)),
    ?assertEqual(not_found,
                 jsxd:get(<<"float">>, Obj)),
    ?assertEqual(not_found,
                 jsxd:get(1, Obj)),
    ?assertEqual({ok, 20},
                 jsxd:get([<<"b">>, 1], Obj)),
    ?assertEqual({ok, 11},
                 jsxd:get([<<"obj">>, <<"a">>], Obj)).

set_arr_test() ->
    SubArr = [10,20,30],
    Arr = [1,SubArr,3],
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
    Obj = [{<<"b">>, SubArr}, {<<"obj">>, SubObj}, {<<"int">>, 1}],

    ?assertEqual([{<<"b">>, SubArr}, {<<"obj">>, SubObj}, {<<"int">>, 2}],
                 jsxd:set(<<"int">>, 2, Obj)),

    ?assertEqual([{<<"b">>, [10,99,30]}, {<<"obj">>, SubObj}, {<<"int">>, 1}],
                 jsxd:set([<<"b">>, 1], 99, Obj)),

    ?assertEqual([{<<"b">>, SubArr}, {<<"obj">>, [{<<"a">>, 99}, {<<"b">>, 12}]}, {<<"int">>, 1}],
                 jsxd:set([<<"obj">>, <<"a">>], 99, Obj)),

    ?assertEqual([{<<"x">>, 1}],
                 jsxd:set(<<"x">>, 1, [])),

    ?assertEqual([{<<"x">>, [{<<"y">>, 1}]}],
                 jsxd:set([<<"x">>, <<"y">>], 1, [])),

    ?assertEqual([{<<"b">>, SubArr}, {<<"obj">>, [{<<"a">>, 11}, {<<"b">>, 12}, {<<"c">>, 99}]}, {<<"int">>, 1}],
                 jsxd:set([<<"obj">>, <<"c">>], 99, Obj)),

    ?assertEqual([{<<"b">>, SubArr}, {<<"obj">>, [{<<"a">>, 11}, {<<"b">>, 12}, {<<"c">>, [{<<"c">>, 99}]}]}, {<<"int">>, 1}],
                 jsxd:set([<<"obj">>, <<"c">>, <<"c">>], 99, Obj)),

    ok.

update_arr_test() ->
    SubArr = [10,20,30],
    Arr = [1,SubArr,3],
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
    Obj = [{<<"b">>, SubArr}, {<<"obj">>, SubObj}, {<<"int">>, 1}],
    Inc = fun(X) -> X + 1 end,
    ?assertEqual([{<<"b">>, SubArr}, {<<"obj">>, SubObj}, {<<"int">>, 2}],
                 jsxd:update(<<"int">>, Inc , Obj)),
    ?assertEqual([{<<"b">>, [10,21,30]}, {<<"obj">>, SubObj}, {<<"int">>, 1}],
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
    Arr = [1,SubArr,3],
    ?assertEqual([SubArr, 3],
                 jsxd:delete(0, Arr)),
    ok.

delete_obj_test() ->
    SubArr = [10,20,30],
    SubObj = [{<<"a">>, 11}, {<<"b">>, 12}],
    Obj = [{<<"b">>, SubArr}, {<<"obj">>, SubObj}, {<<"int">>, 1}],
    ?assertEqual([{<<"b">>, SubArr}, {<<"obj">>, SubObj}],
                 jsxd:delete(<<"int">> , Obj)),
    ok.
map_arr_test() ->
    Arr = [1,3,6],
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

reduce_arr_test() ->
    Arr = [1,3,6],
    ?assertEqual(0*1+1*3+2*6,
                 jsxd:reduce(fun(Idx, Val, Acc) ->
                                     (Idx*Val) + Acc
                          end, 0, Arr)),
    ok.

reduce_obj_test() ->
    Obj = [{<<"a">>,1},{<<"b">>, 2},{<<"c">>, 3}],
    ?assertEqual(1+2+3,
                 jsxd:reduce(fun(_K, Val, Acc) ->
                                  Val + Acc
                          end, 0, Obj)),
    ok.


-endif.

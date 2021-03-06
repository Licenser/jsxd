-module(jsxd).

-export([new/0,
         from_list/1,
         get/2,
         get/3,
         select/2,
         set/3,
         delete/2,
         default/3,
         update/3,
         update/4,
         append/3,
         prepend/3,
         map/2,
         fold/3,
         merge/2,
         merge/3,
         thread/2]).

-type map_key() :: binary()|atom().

-type key()::integer()|map_key().

-type keys()::key()|[key()].

-type value()::binary()|number()|object()|jsxarray()|null|true|false.

-type object()::[{map_key(), value()}] | map().

-type jsxarray()::[value()] | [{}].

-export_type([key/0, keys/0, value/0, object/0, jsxarray/0]).

-spec from_list(value()) -> value().

-define(IS_KEY(Key), (is_binary(Key) orelse is_atom(Key))).

%%--------------------------------------------------------------------
%% @doc Converts a tuple list (or 'array' list) to a object jsxd
%% understands.
%%
%% Maps can be passed in directly without needing conversion.
%% @end
%%--------------------------------------------------------------------

from_list([{_,_}|_] = Obj) ->
    Obj1 = maps:from_list(Obj),
    maps:map(fun (_, E) ->
                     jsxd:from_list(E)
             end, Obj1);

from_list(Obj) when is_list(Obj) ->
    [jsxd:from_list(O) || O <- Obj];

from_list(Obj) ->
    Obj.

%%--------------------------------------------------------------------
%% @doc Creates an empty jsxd structure.
%% @end
%%--------------------------------------------------------------------
-spec new() -> jsxarray() | object().
new() ->
    [].

%%--------------------------------------------------------------------
%% @doc Gets a possibly nested key in a jsxd structure, if the key
%% is a binary it is assumed to be 'unparsed' and will be translated.
%%
%% To set a single binary key direcntly pass in [Key].
%%
%% If the key is not found the default value is returned.
%% @end
%%--------------------------------------------------------------------
-spec get(Key::keys(), Default::value(), Obj::object()|jsxarray()) -> value().

get(Key, Default, Obj) ->
    case get(Key, Obj) of
        {ok, Val} ->
            Val;
        _ ->
            Default
    end.

%%--------------------------------------------------------------------
%% @doc Gets a possibly nested key in a jsxd structure, if the key
%% is a binary it is assumed to be 'unparsed' and will be translated.
%%
%% To set a single binary key direcntly pass in [Key].
%%
%% If the key is not found undefined is returned.
%% @end
%%--------------------------------------------------------------------
-spec get(Key::keys(), Obj::object()|jsxarray()) -> {ok, value()} | undefined.
get(Key, Obj) when (is_list(Obj) orelse is_map(Obj)),
                   is_binary(Key) ->
    get(parse_path(Key), Obj);

get(Key, Obj) when is_list(Obj),
                   (is_integer(Key) orelse is_atom(Key)) ->
    get([Key], Obj);

get([], Obj) ->
    {ok, Obj};

get(_, [{}]) ->
    undefined;

get(_Key, #{}) when is_integer(_Key) ->
    undefined;

get([Pos], [H | _T] = Arr) when is_integer(Pos),
                                not is_tuple(H)->
    try lists:nth(Pos + 1, Arr) of
        Value ->
            {ok, Value}
    catch
        _:_ ->
            undefined
    end;

get([Key], Obj) when ?IS_KEY(Key),
                     is_map(Obj) ->
    case maps:find(Key, Obj) of
        error ->
            undefined;
        R ->
            R
    end;

get([Key], [{_, _} | _T] = Obj)
  when is_binary(Key) orelse is_atom(Key) ->
    case lists:keyfind(Key, 1, Obj) of
        {Key, Value} ->
            {ok, Value};
        _ ->
            undefined
    end;


%% This faults when the key has a wrong format.
get([_Key], _Obj) ->
    undefined;

get([Key | Keys], Obj) when (is_integer(Key) orelse is_atom(Key)
                             orelse is_binary(Key)),
                            (is_list(Obj) orelse is_map(Obj)) ->
    case jsxd:get([Key], Obj) of
        {ok, Obj1} when is_list(Obj1); is_map(Obj1) ->
            jsxd:get(Keys, Obj1);
        _ ->
            undefined
    end.

%%--------------------------------------------------------------------
%% @doc Selects a set of key from a jsxd structure, discarding all
%% others. This does not work in nested fasion!
%% @end
%%--------------------------------------------------------------------
-spec select(Keys::[key()], Obj::object()) -> Obj::object().
select(Keys, Obj) when is_map(Obj) ->
    maps:with(Keys, Obj);
select(Keys, Obj) ->
    select_int(ordsets:from_list(Keys), Obj).

select_int([], _) ->
    jsxd:new();

select_int(_, []) ->
    jsxd:new();

select_int([Key | Keys], [{Key, V} | Obj]) ->
    [{Key, V} | select_int(Keys, Obj)];

select_int([Key1 | Keys], [{Key2, _} | _] = Obj) when Key1 < Key2 ->
    select_int(Keys, Obj);

select_int([Key1 | _] = Keys, [{Key2, _} | Obj]) when Key1 > Key2 ->
    select(Keys, Obj).

%%--------------------------------------------------------------------
%% @doc Sets a value of a jsxd structure, the key behaves the same way
%% os it does for get/2.
%% @end
%%--------------------------------------------------------------------
set([], Val, _Obj) ->
    Val;

set(Key, Val, Obj) when is_integer(Key)->
    set([Key], Val, Obj);

set(Key, Val, Obj) when is_binary(Key) ->
    set(parse_path(Key), Val, Obj);

set([Pos], Val, [H | _T] = Arr) when is_integer(Pos),
                                     not is_tuple(H) ->
    set_arr(Pos, Val, Arr);

set([Pos], Val, [] = Arr) when is_integer(Pos) ->
    set_arr(Pos, Val, Arr);

set([Key], Val, Obj) when ?IS_KEY(Key), is_map(Obj) ->
    Obj#{Key => Val};

set([Key], Val, [{_, _} | _T] = Obj) when ?IS_KEY(Key) ->
    ordsets:add_element({Key, Val},
                        lists:keydelete(Key, 1, Obj));

set([Key], Val, [{}]) when is_binary(Key) ->
    #{Key => Val};

set([Key], Val, []) when is_binary(Key) ->
    #{Key => Val};

set([Key | [_ | _] = Keys], Value, Obj) ->
    SubE = jsxd:get([Key], jsxd:new(), Obj),
    NewE = jsxd:set(Keys, Value, SubE),
    jsxd:set([Key], NewE , Obj).

%%--------------------------------------------------------------------
%% @doc Deletes a value from a jsxd structure, the key behaves the
%% same way os it does for get/2.
%% @end
%%--------------------------------------------------------------------
delete(Key, Obj) when is_integer(Key)->
    delete([Key], Obj);

delete(Key, Obj) when is_binary(Key)->
    delete(parse_path(Key), Obj);


delete([Key], Obj) when (is_binary(Key) orelse is_atom(Key)),
                        is_map(Obj) ->
    case maps:is_key(Key, Obj) of
        true ->
            maps:remove(Key, Obj);
        false ->
            Obj
    end;

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

%%--------------------------------------------------------------------
%% @doc Sets a default for a key, if the key exists this has no
%% effect.
%% @end
%%--------------------------------------------------------------------
default(Keys, Value, Obj) ->
    update(Keys, fun(X) -> X end, Value, Obj).

%%--------------------------------------------------------------------
%% @doc Updates a value from a jsxd structure using a function, the
%% key behaves the same way os it does for get/2.
%%
%% If the key does not exist no change is applied.
%% @end
%%--------------------------------------------------------------------
update(Keys, UpdateFn, Obj) ->
    case jsxd:get(Keys, Obj) of
        undefined ->
            Obj;
        {ok, Val} ->
            jsxd:set(Keys, UpdateFn(Val), Obj)
    end.

%%--------------------------------------------------------------------
%% @doc Updates a value from a jsxd structure using a function, the
%% key behaves the same way os it does for get/2.
%%
%% If the key does not exist the default value is set as new value.
%% @end
%%--------------------------------------------------------------------
update(Keys, UpdateFn, Default, Obj) ->
    case jsxd:get(Keys, Obj) of
        undefined ->
            jsxd:set(Keys, Default, Obj);
        {ok, Val} ->
            jsxd:set(Keys, UpdateFn(Val), Obj)
    end.

%%--------------------------------------------------------------------
%% @doc Appends a value to a jsxd structure. The value appended to
%% must be either undefined or a 'array'.
%% @end
%%--------------------------------------------------------------------
append(Keys, Value, Obj) ->
    jsxd:update(Keys, fun([{_,_} | _]) ->
                              error(bad_argument);
                         (L) when is_list(L) ->
                              L ++ [Value];
                         (_) ->
                              error(bad_argument)
                      end, [Value], Obj).

%%--------------------------------------------------------------------
%% @doc Prepends a value to a jsxd structure. The value appended to
%% must be either undefined or a 'array'.
%% @end
%%--------------------------------------------------------------------
prepend(Keys, Value, Obj) ->
    jsxd:update(Keys, fun([{_,_} | _]) ->
                              error(bad_argument);
                         (L) when is_list(L) ->
                              [Value | L];
                         (_) ->
                              error(bad_argument)
                      end, [Value], Obj).


%%--------------------------------------------------------------------
%% @doc Maps each value of a jsxd structure using a function.
%%
%% The key is either the map key or the array index. However while
%% passed into the function the key can not be changed.
%% @end
%%--------------------------------------------------------------------
map(MapFn, Obj) when is_map(Obj) ->
    maps:map(MapFn, Obj);
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


%%--------------------------------------------------------------------
%% @doc Folds over an jsxd structure.
%%
%% The key is either the map key or the array index.
%% @end
%%--------------------------------------------------------------------
fold(FoldFn, Acc0, Obj) when is_map(Obj) ->
    maps:fold(FoldFn, Acc0, Obj);

fold(FoldFn, Acc0, [{_, _} | _] = Obj) ->
    lists:foldl(fun ({K, V}, Acc) ->
                        FoldFn(K, V, Acc)
                end, Acc0, Obj);

fold(FoldFn, Acc0, Obj) ->
    {_, Res} = lists:foldl(fun (Elem, {I, Acc}) ->
                                   {I + 1, FoldFn(I, Elem, Acc)}
                           end, {0, Acc0}, Obj),
    Res.

%%--------------------------------------------------------------------
%% @doc Merges two objects, where the 2nd objects overwrites values
%% in the first object.
%%
%% The object can not be an array.
%% @end
%%--------------------------------------------------------------------
-spec merge(object(), object()) -> object().
merge(Obj1, Obj2) when is_map(Obj1), is_map(Obj2) ->
    maps:merge(Obj2, Obj1);

merge(Obj1, Obj2) ->
    merge(fun(_,V,_) -> V end, Obj1, Obj2).

%%--------------------------------------------------------------------
%% @doc Merges two objects, using a given function to resolve
%% conflicts.
%%
%% The object can not be an array.
%% @end
%%--------------------------------------------------------------------
merge(ConflictFn, Obj1, Obj2) when is_map(Obj1) ->
    merge(ConflictFn, lists:sort(maps:to_list(Obj1)), Obj2);
merge(ConflictFn, Obj1, Obj2) when is_map(Obj2) ->
    merge(ConflictFn, Obj1, lists:sort(maps:to_list(Obj2)));
merge(ConflictFn, Obj1, Obj2) ->
    maps:from_list(acc_merge(ConflictFn, Obj1, Obj2, [])).

acc_merge(_ConflictFn, [], [], ObjAcc) ->
    ordsets:from_list(ObjAcc);

acc_merge(ConflictFn, [{K1, V1}|Obj1], [{K2, _}|_] = Obj2, ObjAcc) when K1 < K2 ->
    acc_merge(ConflictFn, Obj1, Obj2, [{K1, V1} | ObjAcc]);

acc_merge(ConflictFn, [{K1, V1}|Obj1], [{K1, V2}|Obj2], ObjAcc) ->
    acc_merge(ConflictFn, Obj1, Obj2, [{K1, ConflictFn(K1, V1, V2)} | ObjAcc]);

acc_merge(ConflictFn, [], Obj2, ObjAcc) ->
    acc_merge(ConflictFn, [], [], ObjAcc ++ Obj2);

acc_merge(ConflictFn, [{}], Obj2, ObjAcc) ->
    acc_merge(ConflictFn, [], [], ObjAcc ++ Obj2);

acc_merge(ConflictFn, Obj1, [{K2, V2}|Obj2], ObjAcc) ->
    acc_merge(ConflictFn, Obj1, Obj2, [{K2, V2} | ObjAcc]);

acc_merge(ConflictFn, Obj1, [{}], ObjAcc) ->
    acc_merge(ConflictFn,[], [], ObjAcc ++ Obj1);

acc_merge(ConflictFn, Obj1, [], ObjAcc) ->
    acc_merge(ConflictFn,[], [], ObjAcc ++ Obj1).

%%--------------------------------------------------------------------
%% @doc Threads mutliple changes to jsxd structure, the result from
%% the nth opperation is passed in as new object in the nth+1
%% opperation.
%% @end
%%--------------------------------------------------------------------
thread([], Obj) ->
    Obj;

thread([{select, Ks}|As], Obj) ->
    thread(As, jsxd:select(Ks, Obj));

thread([{set, K, V}|As], Obj) ->
    thread(As, jsxd:set(K, V, Obj));

thread([{append, K, V}|As], Obj) ->
    thread(As, jsxd:append(K, V, Obj));

thread([{prepend, K, V}|As], Obj) ->
    thread(As, jsxd:append(K, V, Obj));

thread([{delete, K}|As], Obj) ->
    thread(As, jsxd:delete(K, Obj));

thread([{default, K, V}|As], Obj) ->
    thread(As, jsxd:default(K, V, Obj));

thread([{update, K, Fn}|As], Obj) ->
    thread(As, jsxd:update(K, Fn, Obj));

thread([{update, K, Fn, Dflt}|As], Obj) ->
    thread(As, jsxd:update(K, Fn, Dflt, Obj));

thread([{merge, Obj1}|As], Obj) ->
    thread(As, jsxd:merge(Obj1, Obj));

thread([{merge,ConflictFn, Obj1}|As], Obj) ->
    thread(As, jsxd:merge(ConflictFn, Obj1, Obj));

thread([{map, Fn}|As], Obj) ->
    thread(As, jsxd:map(Fn, Obj)).

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

parse_path(P) ->
    lists:map(fun ([$[ | N]) ->
                      {X, []} = string:to_integer(N),
                      X;
                  (E) ->
                      list_to_binary(E)
              end,
              lists:filter(fun (E) -> E =/= [] end,
                           re:split(P, "\\.|(?:(\\[\\d)\\])", [trim, {return, list}]))).

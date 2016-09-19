-module(jsxd_eqc).

-include_lib("eqc/include/eqc.hrl").
-compile(export_all).


%%%===================================================================
%%% Tests
%%%===================================================================


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


double() ->
    ?LET(I, int(), I/100).

number() ->
    oneof([double(), int()]).

value() ->
    ?SIZED(Size, value(Size)).

value(0) ->
    oneof([binary(), number(), null, true, false]);

value(S) ->
    ?LAZY(frequency(
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
            jsxd:get(K, jsxd:delete(K, O)) =:= undefined).

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

-module(jsxd_filter_eqc).
-include_lib("eqc/include/eqc.hrl").

-compile(export_all).


path_element() ->
    oneof([
           binary(),
           int(),
           real()
          ]).

path() ->
    non_empty(list(path_element())).



op() ->
    {oneof(['==', '>=', '=<', '>', '<']),
     path(),
     path_element()}.

filter() ->
    ?SIZED(Size, filter(Size)).

filter(0) ->
    ?LAZY(frequency(
            [{5, op()},
             {1, {'~=', path(), binary()}}]));
filter(S) ->
    ?LAZY(
       oneof(
         [
          {'or', filter(S div 2), filter(S div 2)},
          [filter(S div 2), filter(S div 2)],
          {'not', filter(S - 1)}
         ])).


filters() ->
    list(filter()).

prop_serialize_deserialize() ->
    ?FORALL(F, filter(),
            F =:= jsxd_filter:deserialize(jsxd_filter:serialize(F))).

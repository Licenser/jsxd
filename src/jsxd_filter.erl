%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2016, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 19 Sep 2016 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(jsxd_filter).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([filter/2, serialize/1, deserialize/1]).

-type matcher() ::
        '==' | '>=' | '=<' | '>' | '<' | '~='.
-type matcher_i() ::
        16#81..16#86.

-type path() ::
        binary() | [binary() | integer()].

-type filter() ::
        {matcher(), path(), term()}
        | {defined, path()}.

-type filters() ::
        filter() |
        {'not', filters()} |
        {'or', filters(), filters()} |
        [filter()].

-define(AND,     16#01).
-define(OR,      16#02).
-define(NOT,     16#03).

-define(OP_MASK, 2#01111111).
-define(OP_EQ,   16#81).
-define(OP_GTE,  16#82).
-define(OP_LTE,  16#83).
-define(OP_GT,   16#84).
-define(OP_LT,   16#85).
-define(OP_RE,   16#86).

-define(TYPE_BIN,   1).
-define(TYPE_INT,   2).
-define(TYPE_FLOAT, 3).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Tests a object against a given filter, returns either true
%% or false.
%% @end
%%--------------------------------------------------------------------

-spec filter(map(), filters()) ->
                    boolean().
filter(_, []) ->
    true;
filter(O, [{'or', F1, F2} | R]) ->
    (filter(O, F1) orelse filter(O, F2))
        andalso filter(O, R);
filter(O, [{'not', F} | R]) ->
    (not filter(O, F))
        andalso filter(O, R);
filter(O, [F | R]) ->
    filter_(O, F)
        andalso filter(O, R).

%%--------------------------------------------------------------------
%% @doc Serializes a filter to a binary format for sending and storing
%% @end
%%--------------------------------------------------------------------
-spec serialize(filters() | filter()) ->
                       binary().
serialize(Es) when is_list(Es) ->
    D = << << (serialize(E))/binary >> || E <- Es >>,
    <<(byte_size(D)):32, ?AND, D/binary>>;

serialize({'or', L, R}) ->
    Lb = serialize(L),
    Rb = serialize(R),
    S = byte_size(Lb) + byte_size(Rb),
    <<S:32, ?OR, Lb/binary, Rb/binary>>;
serialize({'not', F}) ->
    D = serialize(F),
    <<(byte_size(D)):32, ?NOT, D/binary>>;

serialize({Op, Path, Value}) ->
    Pb = serialize_path(Path),
    Vb = serialize_val(Value),
    S = byte_size(Pb) + byte_size(Vb),
    <<S:32, (op_id(Op)), Pb/binary, Vb/binary>>.

serialize_path(P) ->
    D = << << (serialize_val(E))/binary >> || E <- P >>,
    <<(byte_size(D)):32, D/binary >>.

%%--------------------------------------------------------------------
%% @doc Deserializes a filter from the binary format to a structure.
%% @end
%%--------------------------------------------------------------------
-spec deserialize(binary()) ->
                       filters() | filter().

deserialize(<<_S:32, ?AND, B:_S/binary>>) ->
    [ deserialize(<<S:32, OP, E/binary>>) || <<S:32, OP, E:S/binary>> <= B ];
deserialize(<<_S:32, ?OR,
              Sl:32, OPl, Bl:Sl/binary,
              R/binary>>) ->
    {'or',
     deserialize(<<Sl:32, OPl, Bl/binary>>),
     deserialize(R)};
deserialize(<<_S:32, ?NOT, B/binary>>) ->
    {'not', deserialize(B)};
deserialize(<<_S:32, Op, Sp:32, P:Sp/binary, _Sv:32, V/binary>>) when
      Op band ?OP_MASK =/= 0 ->
    {op_atom(Op), deserialize_path(P), deserialize_val(V)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

filter_(O, {defined, P}) ->
    jsxd:get(P, O) =/= undefined;
filter_(O, {'==', P, V}) ->
    case jsxd:get(P, O) of
        {ok, V1} ->
            V1 == V;
        _ ->
            false
    end;
filter_(O, {'>=', P, V}) ->
    case jsxd:get(P, O) of
        {ok, V1} ->
            V1 >= V;
        _ ->
            false
    end;
filter_(O, {'>', P, V}) ->
    case jsxd:get(P, O) of
        {ok, V1} ->
            V1 > V;
        _ ->
            false
    end;
filter_(O, {'=<', P, V}) ->
    case jsxd:get(P, O) of
        {ok, V1} ->
            V1 =< V;
        _ ->
            false
    end;
filter_(O, {'<', P, V}) ->
    case jsxd:get(P, O) of
        {ok, V1} ->
            V1 =< V;
        _ ->
            false
    end;
filter_(O, {'~=', P, Re}) ->
    case jsxd:get(P, O) of
        {ok, V1} ->
            re:run(V1, Re, [{capture, none}]) =:= match;
        _ ->
            false
    end.

-spec op_id(matcher()) ->
                   matcher_i().
op_id('>=') ->
    ?OP_GTE;
op_id('=<') ->
    ?OP_LTE;
op_id('>') ->
    ?OP_GT;
op_id('<') ->
    ?OP_LT;
op_id('~=') ->
    ?OP_RE;
op_id('==') ->
    ?OP_EQ.

-spec op_atom(matcher_i()) ->
                   matcher().
op_atom(?OP_GTE) ->
    '>=';
op_atom(?OP_LTE) ->
    '=<';
op_atom(?OP_GT) ->
    '>';
op_atom(?OP_LT) ->
    '<';
op_atom(?OP_RE) ->
    '~=';
op_atom(?OP_EQ) ->
    '=='.

serialize_val(B) when is_binary(B) ->
    <<(byte_size(B)):32, ?TYPE_BIN, B/binary>>;
serialize_val(I) when is_integer(I) ->
    <<8:32, ?TYPE_INT, I:64/signed-integer>>;
serialize_val(F) when is_float(F)  ->
    <<8:32, ?TYPE_FLOAT, F:64/float>>.

deserialize_val(<<?TYPE_BIN, B/binary>>)->
    B;
deserialize_val(<<?TYPE_INT, I:64/signed-integer>>) ->
    I;
deserialize_val(<<?TYPE_FLOAT, F:64/float>>) ->
    F.

deserialize_path(P) ->
    [ deserialize_val(<<T, V/binary>>) || <<_Sv:32, T, V:_Sv/binary>> <= P].



-ifdef(TEST).
eqal_test() ->
    ?assert(filter(#{a => 1}, [{'==', [a], 1}])),
    ?assert(not filter(#{a => 1}, [{'==', [a], 2}])).
serialize_test() ->
    Raw = [{'or',{'not',[{'==',[<<"some">>,1],<<"someone">>}]},[]}],
    ?assertEqual(Raw, deserialize(serialize(Raw))).
-endif.

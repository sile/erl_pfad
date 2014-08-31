%% @doc Chapter16: The Countdown problem
-module(pfad_ch20).

-export([
         countdown1/2,
         countdown2/2,
         countdown3/2,
         countdown4/2,
         countdown5/2
        ]).

-type expr() :: {num, pos_integer()} |
                {app, op(), expr(), expr()}.
-type op() :: '+' | '-' | '*' | 'div'.
-type value() :: pos_integer().

-define(WHEN(Exp, Value),
        case Exp of
            false -> [];
            true  -> Value
        end).

-spec countdown1(pos_integer(), [pos_integer()]) -> {expr(), value()}.
countdown1(N, Numbers) ->
    nearest(N, pfad_util:concat_map(fun mk_exprs/1, subseqs(Numbers))).

-spec subseqs([term()]) -> [[term()]].
subseqs([X])      -> [[X]];
subseqs([X | Xs]) ->
    Xss = subseqs(Xs),
    Xss ++ [[X]] ++ [[X | Ys] || Ys <- Xss].

%% -spec value(expr()) -> value().
%% value({num, X})          -> X;
%% value({app, Op, E1, E2}) -> erlang:Op(E1, E2).

-spec legal(op(), value(), value()) -> boolean().
legal('-',   V1, V2) -> V2 < V1;
legal('div', V1, V2) -> V1 rem V2 =:= 0;
legal(_, _, _)       -> true.

-spec mk_exprs([pos_integer()]) -> [{expr(), value()}].
mk_exprs([X]) -> [{{num, X}, X}];
mk_exprs(Xs)  ->
    [Ev || {Ys, Zs} <- unmerges(Xs),
           Ev1 <- mk_exprs(Ys),
           Ev2 <- mk_exprs(Zs),
           Ev <- combine(Ev1, Ev2)].

-spec unmerges([term()]) -> [{[term()], [term()]}].
unmerges([X, Y])   -> [{[X], [Y]}, {[Y], [X]}];
unmerges([X | Xs]) ->
    Add = fun ({Ys, Zs}) -> [{[X | Ys], Zs}, {Ys, [X | Zs]}] end,
    [{[X], Xs}, {Xs, [X]}] ++ pfad_util:concat_map(Add, unmerges(Xs)).

-spec combine({expr(), value()}, {expr(), value()}) -> [{expr(), value()}].
combine({E1, V1}, {E2, V2}) ->
    [{{app, Op, E1, E2}, erlang:Op(V1, V2)} || Op <- ops(), legal(Op, V1, V2)].

-spec ops() -> [op()].
ops() -> ['+', '-', '*', 'div'].

-spec nearest(pos_integer(), [{expr(), value()}]) -> {expr(), value()}.
nearest(N, [{E, V} | Evs]) ->
    D = abs(N - V),
    case D =:= 0 of
        true  -> {E, V};
        false -> search(N, D, {E, V}, Evs)
    end.

-spec search(pos_integer(), pos_integer(), {expr(), value()}, [{expr(), value()}]) -> {expr(), value()}.
search(_, _, Ev, [])             -> Ev;
search(N, D, Ev, [{E, V} | Evs]) ->
    D2 = abs(N - V),
    if
        D2 =:= 0 -> {E, V};
        D2 < D   -> search(N, D2, {E, V}, Evs);
        D2 >= D  -> search(N, D, Ev, Evs)
    end.

-spec p159_unmerges([term()]) -> [{[term()], [term()]}].
p159_unmerges([X, Y])   -> [{[X], [Y]}];
p159_unmerges([X | Xs]) ->
    Add = fun ({Ys, Zs}) -> [{[X | Ys], Zs}, {Ys, [X | Zs]}] end,
    [{[X], Xs}] ++ pfad_util:concat_map(Add, p159_unmerges(Xs)).

%% -spec p159_legal(op(), value(), value()) -> boolean().
%% p159_legal('+',   V1, V2) -> V1 =< V2;
%% p159_legal('-',   V1, V2) -> V2 < V1;
%% p159_legal('*',   V1, V2) -> (1 < V1) andalso (V1 =< V2);
%% p159_legal('div', V1, V2) -> (1 < V2) andalso (V1 rem V2 =:= 0).

%% -spec p159_combine({expr(), value()}, {expr(), value()}) -> [{expr(), value()}].
%% p159_combine({E1, V1}, {E2, V2}) ->
%%     [{{app, Op, E1, E2}, erlang:Op(V1, V2)} || Op <- ops(), p159_legal(Op, V1, V2)] ++
%%     [{{app, Op, E2, E1}, erlang:Op(V2, V1)} || Op <- ops(), p159_legal(Op, V2, V1)].

-spec countdown2(pos_integer(), [pos_integer()]) -> {expr(), value()}.
countdown2(N, Numbers) ->
    nearest(N, pfad_util:concat_map(fun mk_exprs_2/1, subseqs(Numbers))).

-spec mk_exprs_2([pos_integer()]) -> [{expr(), value()}].
mk_exprs_2([X]) -> [{{num, X}, X}];
mk_exprs_2(Xs)  ->
    [Ev || {Ys, Zs} <- p159_unmerges(Xs),
           Ev1 <- mk_exprs_2(Ys),
           Ev2 <- mk_exprs_2(Zs),
           Ev <- p160_combine(Ev1, Ev2)].

-spec p160_combine({expr(), value()}, {expr(), value()}) -> [{expr(), value()}].
p160_combine({E1, V1}, {E2, V2}) ->
    if
        V1 < V2   -> comb1({E1, V1}, {E2, V2});
        V1 =:= V2 -> comb2({E1, V1}, {E2, V2});
        V1 > V2   -> comb1({E2, V2}, {E1, V1})
    end.

-spec comb1({expr(), value()}, {expr(), value()}) -> [{expr(), value()}].
comb1({E1, V1}, {E2, V2}) ->
    [{{app, '+', E1, E2}, V1 + V2},
     {{app, '-', E2, E1}, V2 - V1}] ++
    ?WHEN(1 < V1,
          [{{app, '*', E1, E2}, V1 * V2}] ++
          ?WHEN(V2 rem V1 =:= 0, [{{app, 'div', E2, E1}, V2 div V1}])).

-spec comb2({expr(), value()}, {expr(), value()}) -> [{expr(), value()}].
comb2({E1, V1}, {E2, V2}) ->
    [{{app, '+', E1, E2}, V1 + V2}] ++
    ?WHEN(1 < V1,
          [{{app, '*', E1, E2}, V1 * V2}, {{app, 'div', E1, E2}, 1}]).

-spec non(op(), expr()) -> boolean().
non(_Op, {num, _})         -> true;
non(Op1, {app, Op2, _, _}) -> Op1 =/= Op2.

%% -spec p161_legal(op(), value(), value()) -> boolean().
%% p161_legal('+',   V1, V2) -> V1 =< V2 andalso non('-', E1) andalso non('+', E2) andalso non('-', E2);
%% p161_legal('-',   V1, V2) -> V2 < V1 andalso non('-', E1) andalso non('-', E2);
%% p161_legal('*',   V1, V2) -> (1 < V1) andalso (V1 =< V2) andalso non('div', E1) andalso non('*', E2) andalso non('div', E2);
%% p161_legal('div', V1, V2) -> (1 < V2) andalso (V1 rem V2 =:= 0) andalso non('div', E1) andalso non('div', E2).

-spec countdown3(pos_integer(), [pos_integer()]) -> {expr(), value()}.
countdown3(N, Numbers) ->
    nearest(N, pfad_util:concat_map(fun mk_exprs_3/1, subseqs(Numbers))).

-spec mk_exprs_3([pos_integer()]) -> [{expr(), value()}].
mk_exprs_3([X]) -> [{{num, X}, X}];
mk_exprs_3(Xs)  ->
    [Ev || {Ys, Zs} <- p159_unmerges(Xs),
           Ev1 <- mk_exprs_3(Ys),
           Ev2 <- mk_exprs_3(Zs),
           Ev <- p162_combine(Ev1, Ev2)].

-spec p162_combine({expr(), value()}, {expr(), value()}) -> [{expr(), value()}].
p162_combine({E1, V1}, {E2, V2}) ->
    if
        V1 < V2   -> p162_comb1({E1, V1}, {E2, V2});
        V1 =:= V2 -> p162_comb2({E1, V1}, {E2, V2});
        V1 > V2   -> p162_comb1({E2, V2}, {E1, V1})
    end.

-spec p162_comb1({expr(), value()}, {expr(), value()}) -> [{expr(), value()}].
p162_comb1({E1, V1}, {E2, V2}) ->
    ?WHEN(non('-', E1) andalso non('-', E2),
          ?WHEN(non('+', E2), [{{app, '+', E1, E2}, V1 + V2}]) ++
          [{{app, '-', E2, E1}, V2 - V1}]) ++
    ?WHEN(1 < V1 andalso non('div', E1) andalso non('div', E2),
          ?WHEN(non('*', E2),    [{{app, '*', E1, E2}, V1 * V2}]) ++
          ?WHEN(V2 rem V1 =:= 0, [{{app, 'div', E2, E1}, V2 div V1}])).

-spec p162_comb2({expr(), value()}, {expr(), value()}) -> [{expr(), value()}].
p162_comb2({E1, V1}, {E2, V2}) ->
    ?WHEN(non('-', E1) andalso non('+', E2) andalso non('-', E2),
          [{{app, '+', E1, E2}, V1 + V2}]) ++
    ?WHEN(1 < V1 andalso non('div', E1) andalso non('div', E2),
          ?WHEN(non('*', E2), [{{app, '*', E1, E2}, V1 * V2}]) ++
          [{{app, 'div', E1, E2}, 1}]).

-spec p163_mk_exprs(memo(), [pos_integer()]) -> [{expr(), value()}].
p163_mk_exprs(_, [X])   -> [{{num, X}, X}];
p163_mk_exprs(Memo, Xs) ->
    [Ev || {Ys, Zs} <- p159_unmerges(Xs),
           Ev1 <- trie_fetch(Memo, Ys),
           Ev2 <- trie_fetch(Memo, Zs),
           Ev <- p162_combine(Ev1, Ev2)].

-spec countdown4(pos_integer(), [pos_integer()]) -> {expr(), value()}.
countdown4(N, Numbers) ->
    nearest(N, extract(memoize(subseqs(Numbers)))).

-spec memoize([[pos_integer()]]) -> memo().
memoize(Input) ->
    Insert =
        fun (Xs, Memo) ->
                trie_store(Xs, p163_mk_exprs(Memo, Xs), Memo)
        end,
    lists:foldl(Insert, trie_empty(), Input).

-type trie(A) :: {node, A, [{pos_integer(), trie(A)}]}.
-type trie() :: trie(term()).

-type memo() :: trie([{expr(), value()}]).

-spec trie_empty() -> trie().
trie_empty() ->
    {node, [], []}.

-spec trie_fetch(trie(), [pos_integer()]) -> [{expr(), value()}].
trie_fetch({node, Es, _}, [])        -> Es;
trie_fetch({node, _, Xms}, [X | Xs]) ->
    {_, Follow} = lists:keyfind(X, 1, Xms),
    trie_fetch(Follow, Xs).

-spec trie_store([pos_integer()], [{expr(), value()}], trie()) -> trie().
trie_store([X], Es, {node, Fs, Xms}) ->
    {node, Fs, [{X, {node, Es, []}} | Xms]};
trie_store([X | Xs], Es, {node, Fs, Xms}) ->
    {Yms, [{_, M} | Zms]} = lists:splitwith(fun ({Z, _}) -> X =/= Z end, Xms),
    {node, Fs, Yms ++ [{X, trie_store(Xs, Es, M)} | Zms]}.

-spec extract(memo()) -> [{expr(), value()}].
extract({node, Es, Xms}) ->
    Es ++ pfad_util:concat_map(fun ({_, M}) -> extract(M) end, Xms).

-type tree() :: {tip, pos_integer()} | {bin, tree(), tree()}.
-type tree_memo() :: trie([tree()]).

-spec countdown5(pos_integer(), [pos_integer()]) -> {expr(), value()}.
countdown5(N, Numbers) ->
    nearest(N, pfad_util:concat_map(fun to_exprs/1, extract(memoize2(subseqs(Numbers))))).

-spec memoize2([[pos_integer()]]) -> tree_memo().
memoize2(Input) ->
    Insert =
        fun (Xs, Memo) ->
                trie_store(Xs, mk_trees(Memo, Xs), Memo)
        end,
    lists:foldl(Insert, trie_empty(), Input).

-spec mk_trees(tree_memo(), [pos_integer()]) -> [tree()].
mk_trees(_, [X])   -> [{tip, X}];
mk_trees(Memo, Xs) ->
    [{bin, T1, T2} || {Ys, Zs} <- p159_unmerges(Xs),
                      T1 <- trie_fetch(Memo, Ys),
                      T2 <- trie_fetch(Memo, Zs)].

-spec to_exprs(tree()) -> [{expr(), value()}].
to_exprs({tip, X})      -> [{{num, X}, X}];
to_exprs({bin, T1, T2}) ->
    [Ev || Ev1 <- to_exprs(T1),
           Ev2 <- to_exprs(T2),
           Ev <- p162_combine(Ev1, Ev2)].

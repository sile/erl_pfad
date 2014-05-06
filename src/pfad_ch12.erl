%% @doc Chapter12: Ranking suffixes
-module(pfad_ch12).

-export([
         ranktails/1,
         n_log2_n_ranktails/1,
         n_log_n_ranktails/1
        ]).

-spec ranktails(string()) -> [non_neg_integer()].
ranktails(Xs) ->
    rank(tails(Xs)).

-spec tails(string()) -> [string()].
tails([]) -> [];
tails(Xs) -> [Xs | tails(tl(Xs))].

-spec rank([term()]) -> [non_neg_integer()].
rank(Xs) ->
    [length(lists:filter(fun (Y) -> Y < X end, Xs)) || X <- Xs].

-spec n_log2_n_ranktails(string()) -> [non_neg_integer()].
n_log2_n_ranktails(Xs) ->
    apply_until(fun isperm/1, rerankings(1), n_log_n_rank(Xs)).

-spec apply_until(PredFun, NextFun, State) -> State when
      PredFun :: fun ((State) -> boolean()),
      NextFun :: fun ((State) -> {State, NextFun}),
      State   :: term().
apply_until(PredFun, NextFun, State) ->
    case PredFun(State) of
        true  -> State;
        false ->
            {State2, NextFun2} = NextFun(State),
            apply_until(PredFun, NextFun2, State2)
    end.

-spec isperm([non_neg_integer()]) -> boolean().
isperm(Is) ->
    AllTrue = (1 bsl length(Is)) - 1,
    AllTrue =:= lists:foldl(fun (R, Acc) -> Acc bor (1 bsl R) end, 0, Is).

-spec rerankings(non_neg_integer()) -> Fun when
      Fun   :: fun ((Ranks) -> {Ranks, Fun}),
      Ranks :: [non_neg_integer()].
rerankings(K) ->
    fun (Ranks) ->
            {rerank(K, Ranks), rerankings(K * 2)}
    end.

-spec rerank(non_neg_integer(), [non_neg_integer()]) -> [non_neg_integer()].
rerank(K, Ranks) ->
    '<<'(Ranks, shift_by(K, Ranks)).

-spec '<<'([non_neg_integer()], [non_neg_integer()]) -> [non_neg_integer()].
'<<'(Ranks1, Ranks2) ->
    rank(lists:zip(Ranks1, Ranks2)).

-spec shift_by(non_neg_integer(), [non_neg_integer()]) -> [non_neg_integer()].
shift_by(K, Ranks) ->
    lists:map(fun (N) -> N + K end, drop(K, Ranks)) ++ lists:seq(K - 1, 0, -1).

-spec drop(non_neg_integer(), [term()]) -> [term()].
drop(0, List)     -> List;
drop(N, [_ | Xs]) -> drop(N - 1, Xs).

-spec n_log_n_rank([term()]) -> [non_neg_integer()].
n_log_n_rank(Xs) ->
    resort(lists:append(label(psort(partition(Xs))))).

-spec psort([{non_neg_integer(), term()}]) -> [[non_neg_integer()]].
psort([]) -> [];
psort(Xs) ->
    Xs2 = lists:keysort(2, Xs),
    element(2,
        lists:foldr(
          fun ({I, X}, undefined)          -> {X, [[I]]};
              ({I, X}, {X, [Head | Tail]}) -> {X, [[I | Head] | Tail]};
              ({I, X}, {_, List})          -> {X, [[I] | List]}
          end,
          undefined,
          Xs2)).

-spec label([[term()]]) -> [[{term(), non_neg_integer()}]].
label(Xss) ->
    lists:zipwith(fun tag/2, Xss, pfad_util:scanl(fun erlang:'+'/2, 0, [length(Xs) || Xs <- butlast(Xss)])).

-spec tag([term()], non_neg_integer()) -> [{term(), non_neg_integer()}].
tag(Xs, K) ->
    [{X, K} || X <- Xs].

-spec resort([{non_neg_integer(), non_neg_integer()}]) -> [non_neg_integer()].
resort(Ijs) ->
    tuple_to_list(make_array(length(Ijs), Ijs)).

-spec butlast([term()]) -> [term()].
butlast([_])      -> [];
butlast([X | Xs]) -> [X | butlast(Xs)].

-spec partition([term()]) -> [[non_neg_integer()]].
partition(List) ->
    psort(lists:zip(lists:seq(0, length(List) - 1), List)).

-spec n_log_n_ranktails(string()) -> [non_neg_integer()].
n_log_n_ranktails(Xs) ->
    resort(lists:append(label(apply_until(fun all_single/1, repartitions(length(Xs), 1), partition(Xs))))).

-spec all_single([[term()]]) -> boolean().
all_single(ListOfList) ->
    lists:all(fun ([_]) -> true; (_) -> false end, ListOfList).

-spec repartitions(non_neg_integer(), non_neg_integer()) -> Fun when
      Fun       :: fun (([Partition]) -> {[Partition], Fun}),
      Partition :: [non_neg_integer()].
repartitions(N, K) ->
    fun (Iss) ->
            {repartition(N, K, Iss), repartitions(N, K * 2)}
    end.

-spec repartition(non_neg_integer(), non_neg_integer(), [[non_neg_integer()]]) -> [[non_neg_integer()]].
repartition(N, K, Iss) ->
    A = make_array(N, lists:append(label(Iss))),
    Install =
        fun (I) ->
                J = I + K,
                V = case J < N of
                        false -> N - I - 1;
                        true  -> K + element(J + 1, A)
                    end,
                {I, V}
        end,
    lists:flatmap(fun (Is) -> psort(lists:map(Install, Is)) end, Iss).

-spec make_array(non_neg_integer(), [{non_neg_integer(), term()}]) -> tuple().
make_array(Size, InitList) ->
    erlang:make_tuple(Size, 0, [{K + 1, V} || {K, V} <- InitList]).

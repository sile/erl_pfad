% @doc Chapter13: The Burrows-Wheeler transform
-module(pfad_ch13).

-export([
         transform/1,
         untransform/1,
         n_log_n_untransform/1
        ]).

-spec transform([term()]) -> {[term()], non_neg_integer()}.
transform(Xs) ->
    Xss = lists:sort(rots(Xs)),
    {lists:map(fun lists:last/1, Xss), position(Xs, Xss)}.

-spec position(term(), [term()]) -> non_neg_integer().
position(X, Xs) ->
    length(lists:takewhile(fun (Y) -> Y =/= X end, Xs)).

-spec rots([term()]) -> [[term()]].
rots(Xs) ->
    Lrot = fun ([Y | Ys]) -> Ys ++ [Y] end,
    lists:foldl(
      fun (_, [Head | _] = Acc) -> [Lrot(Head) | Acc] end,
      [Xs],
      lists:seq(2, length(Xs))).

-spec untransform({[term()], non_neg_integer()}) -> [term()].
untransform({Ys, K}) ->
    lists:nth(K + 1, recreate(length(Ys), Ys)).

%% -spec take_cols(non_neg_integer(), [[term()]]) -> [[term()]].
%% take_cols(J, ListOfList) ->
%%     [lists:sublist(List, J) || List <- ListOfList].

%% -spec rrot([term()]) -> [term()].
%% rrot(Xs) ->
%%     {ButLast, Last} = lists:split(length(Xs) - 1, Xs),
%%     Last ++ ButLast.

-spec hdsort([[term()]]) -> [[term()]].
hdsort(ListOfList) ->
    lists:sort(fun ([X | _], [Y | _]) -> X =< Y end, ListOfList).

-spec cons_col({[term()], [[term()]]}) -> [[term()]].
cons_col({Xs, Xss}) ->
    lists:zipwith(fun (Y, Ys) -> [Y | Ys] end, Xs, Xss).

-spec recreate(non_neg_integer(), [term()]) -> [[term()]].
recreate(0, Xs) -> [[] || _ <- Xs];
recreate(J, Xs) ->
    hdsort(cons_col({Xs, recreate(J - 1, Xs)})).

-spec n_log_n_untransform({[term()], non_neg_integer()}) -> [term()].
n_log_n_untransform({Ys, K}) ->
    N = length(Ys),
    Pa = list_to_tuple([Snd || {_, Snd} <- lists:sort(lists:zip(Ys, lists:seq(1, N)))]),
    Ya = list_to_tuple(Ys),
    tl([element(I, Ya) || I <- iterate(N + 1, fun (I) -> element(I, Pa) end, K + 1)]).

-spec iterate(non_neg_integer(), Fun, term()) -> [term()] when
      Fun :: fun ((term()) -> term()).
iterate(0, _, _)   -> [];
iterate(N, Fun, X) -> [X | iterate(N - 1, Fun, Fun(X))].

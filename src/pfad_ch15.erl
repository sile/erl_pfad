%% @doc Chapter15: All the common prefixes
-module(pfad_ch15).

-export([
         allcp/1,
         linear_allcp/1
        ]).

-spec allcp([term()]) -> [non_neg_integer()].
allcp(List) ->
    [llcp(List, Tail) || Tail <- pfad_util:tails(List)].

-spec llcp([term()], [term()]) -> non_neg_integer().
llcp([X | Xs], [X | Ys]) -> 1 + llcp(Xs, Ys);
llcp(_, _)               -> 0.

%% This function takes linear time under the assumption that
%% each snoc/2, at/2, drop/2 function takes constant time.
-spec linear_allcp([term()]) -> [non_neg_integer()].
linear_allcp(Xs) ->
    N = length(Xs),
    {Result, _, _} =
        lists:foldl(
          fun (K, Acc) -> step(Xs, K, Acc) end,
          {[N], 0, 0},
          lists:seq(1, N - 1)),
    Result.

-spec drop(non_neg_integer(), list()) -> list().
drop(N, List) ->
    lists:nthtail(N, List).

-spec snoc(list(), term()) -> list().
snoc(List, X) ->
    List ++ [X].

-spec at(list(), non_neg_integer()) -> term().
at(List, I) ->
    lists:nth(I + 1, List).

-spec step([term()], non_neg_integer(), {[non_neg_integer()], non_neg_integer(), non_neg_integer()}) ->
                  {[non_neg_integer()], non_neg_integer(), non_neg_integer()}.
step(Xs, K, {As, I, P}) ->
    CalcLlcp = fun (Offset) ->
                       A = Offset + llcp(drop(Offset, Xs), drop(Offset + K, Xs)),
                       {snoc(As, A), K, A}
               end,
    case K >= I + P of
        true  -> CalcLlcp(0);
        false ->
            Q = at(As, K - I),
            R = P - (K - I),
            case Q =/= R of
                true  -> {snoc(As, min(Q, R)), I, P};
                false -> CalcLlcp(Q)
            end
    end.

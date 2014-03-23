%% @doc Chapter14: The last tail
-module(pfad_ch14).

-export([
         maxtail/1,
         linear_maxtail/1
        ]).

-spec maxtail([term()]) -> [term()].
maxtail(List) ->
    lists:max(pfad_util:tails(List)).

-spec linear_maxtail([term()]) -> [term()].
linear_maxtail([])       -> [];
linear_maxtail([X | Xs]) -> step(0, 1, [X | Xs], [X | Xs], Xs).

-spec step(non_neg_integer(), non_neg_integer(), [term()], [term()], [term()]) -> [term()].
step(_, _, Ys, _, [])              -> Ys;
step(P, Q, Ys, [W | Ws], [X | Xs]) ->
    R = P rem Q,
    if
        W < X   -> linear_maxtail(lists:nthtail(Q - R, [W | Ws]));
        W =:= X -> step(P + 1, Q, Ys, Ws, Xs);
        W > X   -> step(0, P + Q + 1, Ys, Ys, Xs)
    end.

            





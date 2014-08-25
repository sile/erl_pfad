%% @doc utility functions
-module(pfad_util).

-export([
         count_if/2,
         map_tail/2,
         min_by/2,
         fork/2,
         id/1,
         tails/1,
         inits/1,
         fst/1,
         snd/1,
         scanl/3,
         while/2,
         cons/2,
         take/2,
         drop/2
        ]).

-spec map_tail(Fun, [Element]) -> [Result] when
      Fun     :: fun ((Element, Tail) -> Result),
      Element :: term(),
      Result  :: term(),
      Tail    :: [Element].
map_tail(Fun, List) ->
    {Results, _} =
        lists:mapfoldl(
          fun (Element, PrevTail) ->
                  Tail = tl(PrevTail),
                  Result = Fun(Element, Tail),
                  {Result, Tail}
          end,
          List,
          List),
    Results.

-spec count_if(Fun, [term()]) -> non_neg_integer() when
      Fun :: fun ((term()) -> boolean()).
count_if(Fun, List) ->
    lists:foldl(fun (X, Count) -> boolean_to_integer(Fun(X)) + Count end,
                0,
                List).

-spec boolean_to_integer(boolean()) -> 0|1.
boolean_to_integer(true)  -> 1;
boolean_to_integer(false) -> 0.

-spec min_by(Fun, [X]) -> X when
      Fun :: fun ((X) -> Cost::term()),
      X   :: term().
min_by(Fun, [Head | Tail]) ->
    {_, Min} =
        lists:foldl(
          fun (X, {MinCost, MinX}) ->
                  case Fun(X) of
                      Cost when Cost < MinCost -> {Cost, X};
                      _                        -> {MinCost, MinX}
                  end
          end,
          {Fun(Head), Head},
          Tail),
    Min.

-spec id(term()) -> term().
id(X) -> X.

-spec fork({Fun, Fun}, term()) -> {term(), term()} when
      Fun :: fun ((term()) -> term()).
fork({Fun1, Fun2}, X) ->
    {Fun1(X), Fun2(X)}.

-spec tails([term()]) -> [[term()]].
tails([]) -> [];
tails(Xs) -> [Xs | tails(tl(Xs))].

-spec inits([term()]) -> [[term()]].
inits([])       -> [];
inits([X | Xs]) -> [[X] | [[X | Ys] || Ys <- inits(Xs)]].

-spec fst({X, _}) -> X when X :: term().
fst({X, _}) -> X.

-spec snd({_, X}) -> X when X :: term().
snd({_, X}) -> X.

-spec scanl(Fun, Initial, List) -> Acc when
      Fun     :: fun ((term(), term()) -> term()),
      Initial :: term(),
      List    :: [term()],
      Acc     :: [term()].
scanl(Fun, Initial, List) ->
    lists:reverse(
      lists:foldl(fun (X, Acc) -> [Fun(X, hd(Acc)) | Acc] end,
                  [Initial],
                  List)).

-spec while(Fun, Arg) -> Result when
      Fun    :: fun ((Arg) -> {boolean(), Result}),
      Arg    :: term(),
      Result :: term().
while(Fun, Arg) ->
    case Fun(Arg) of
        {false, Result} -> Result;
        {true, NextArg} -> while(Fun, NextArg)
    end.

-spec cons(term(), list()) -> list().
cons(X, Xs) ->
    [X | Xs].

-spec take(non_neg_integer(), list()) -> list().
take(N, List) ->
    lists:sublist(List, 1, N).

-spec drop(non_neg_integer(), list()) -> list().
drop(N, List) ->
    lists:nthtail(N, List).

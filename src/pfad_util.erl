%% @doc utility functions
-module(pfad_util).

-export([
         count_if/2,
         map_tail/2
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

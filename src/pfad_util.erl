%% @doc utility functions
-module(pfad_util).

-export([
         count_if/2,
         map_tail/2,
         concat_map/2,
         min_by/2,
         fork/2,
         id/1,
         tails/1,
         inits/1,
         fst/1,
         snd/1,
         scanl/3,
         scanr/3,
         while/2,
         cons/2,
         take/2,
         drop/2,
         minors/1,
         foldr1/2,
         transpose/1,
         signum/1,
         cycle/2,
         unfoldr/2,
         stream/4,
         destream/6
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

-spec scanr(Fun, Initial, List) -> Acc when
      Fun     :: fun ((term(), term()) -> term()),
      Initial :: term(),
      List    :: [term()],
      Acc     :: [term()].
scanr(Fun, Initial, List) ->
    lists:foldr(fun (X, Acc) -> [Fun(X, hd(Acc)) | Acc] end,
                [Initial],
                List).

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

-spec concat_map(Fun, List1) -> List2 when
      Fun    :: fun ((Input) -> [Output]),
      List1  :: [Input],
      List2  :: [Output],
      Input  :: term(),
      Output :: term().
concat_map(Fun, List) ->
    lists:append(lists:map(Fun, List)).

-spec minors([A]) -> [[A]] when A :: term().
minors([])       -> [];
minors([X | Xs]) -> [Xs | [[X | Ys] || Ys <- minors(Xs)]].

-spec foldr1(Fun, List) -> Result when
      Fun     :: fun ((Element, Acc) -> Result),
      List    :: [Element],
      Element :: term(),
      Acc     :: Element,
      Result  :: Element.
foldr1(Fun, Xs) ->
    lists:foldr(Fun, lists:last(Xs), lists:droplast(Xs)).

-spec transpose([[A]]) -> [[A]] when A :: term().
transpose([])       -> [];
transpose(Rows) ->
    {Col, RestRows} =
        lists:foldr(
          fun ([], Acc) ->
                  Acc;
              ([H | T], {Heads, Tails}) ->
                  {[H | Heads], [T | Tails]}
          end,
          {[], []},
          Rows),
    case Col of
        [] -> [];
        _  -> [Col | transpose(RestRows)]
    end.

-spec signum(number()) -> -1 | 0 | 1.
signum(N) when N > 0 ->  1;
signum(N) when N < 0 -> -1;
signum(_)            ->  0.

-spec cycle([A], non_neg_integer()) -> [A] when A :: term().
cycle(Xs, N) ->
    Cycle = fun Cycle(_, 0)        -> [];
                Cycle([], I)       -> Cycle(Xs, I);
                Cycle([Y | Ys], I) -> [Y | Cycle(Ys, I - 1)]
            end,
    Cycle(Xs, N).

-spec unfoldr(Fun, B) -> [A] when
      Fun :: fun ((B) -> {ok, A, B} | error),
      A   :: term(),
      B   :: term().
unfoldr(Fun, B) ->
    case Fun(B) of
        error       -> [];
        {ok, A, B2} -> [A | unfoldr(Fun, B2)]
    end.

-spec stream(F, G, S, Xs) -> [Value] when
      F :: fun ((S) -> {ok, Value, S} | error),
      G :: fun ((S, X) -> S),
      S :: term(),
      X :: term(),
      Xs :: [X],
      Value :: term().
stream(F, G, S0, Xs0) ->
    Step =
        fun Step({S, Xs}) ->
                case F(S) of
                    {ok, Y, S_} -> {ok, Y, {S_, Xs}};
                    error       ->
                        case Xs of
                            [X | Xs_] -> Step({G(S, X), Xs_});
                            []        -> error
                        end
                end
        end,
    unfoldr(Step, {S0, Xs0}).

-spec destream(N, F, G, H, S, Ys) -> [X] when
      N :: non_neg_integer(),
      F :: fun ((S) -> {ok, Y, S}),
      G :: fun ((S, X) -> S),
      H :: fun ((S, Ys) -> X),
      S :: term(),
      X :: term(),
      Y :: term(),
      Ys :: [Y].
destream(N0, F, G, H, S0, Ys0) ->
    Step =
        fun Step({0, _, _}) ->
                error;
            Step({N, S, Ys}) ->
                case F(S) of
                    {ok, _Y, S_} ->
                        _Y = hd(Ys),
                        Step(S_, tl(Ys));
                    error        ->
                        X = H(S, Ys),
                        {ok, X, {N - 1, G(S, X), Ys}}
                end
        end,
    unfoldr(Step, {N0, S0, Ys0}).

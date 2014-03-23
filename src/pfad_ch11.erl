%% @doc Chapter11: Not the maximum segment sum
-module(pfad_ch11).

-export([
         mnss/1,
         linear_mnss/1
        ]).

-spec mnss([integer()]) -> integer().
mnss(List) -> 
    lists:max(lists:map(fun lists:sum/1, nonsegs(List))).

-spec nonsegs([integer()]) -> [[integer()]].
nonsegs(List) ->
    extract(lists:filter(fun nonseg/1, markings(List))).

-spec markings([term()]) -> [[{term(), boolean()}]].
markings(Xs) ->
    [lists:zip(Xs, Bs) || Bs <- booleans(length(Xs))].

-spec booleans(non_neg_integer()) -> [[boolean()]].
booleans(0) -> [[]];
booleans(N) -> [[B | Bs] || B <- [true, false], Bs <- booleans(N - 1)].

-spec extract([[{term(), boolean()}]]) -> [[term()]].
extract(ListOfList) ->
    [[X || {X, true} <- List] || List <- ListOfList].

-spec nonseg([{integer(), boolean()}]) -> boolean().
nonseg(List) ->
    'N' =:= lists:foldl(fun step/2, 'E', [Snd || {_, Snd} <- List]).

-spec step(boolean(), State) -> State when
      State :: 'E' | 'S' | 'M' | 'N'.
step(false, 'E') -> 'E';
step(true,  'E') -> 'S'; 
step(false, 'S') -> 'M';
step(true,  'S') -> 'S';
step(false, 'M') -> 'M';
step(true,  'M') -> 'N';
step(false, 'N') -> 'N';
step(true,  'N') -> 'N'.

-define(NEGATIVE_INF, -1000000000000000000000000000000000000000000000).% approximate value
-spec linear_mnss([integer()]) -> integer().
linear_mnss(List) ->
    H = fun (X, {E, S, M, N}) ->
                {E, max(S, E) + X, max(M, S), max(N, max(N, M) + X)}
        end,
    element(4, lists:foldl(H, {0, ?NEGATIVE_INF, ?NEGATIVE_INF, ?NEGATIVE_INF}, List)).

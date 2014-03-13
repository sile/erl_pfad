%% @doc Chapter8: Unravelling greedy algorithms
-module(pfad_ch8).

-export([
         supravel/1,
         greedy_supravel/1,
         unravels/1
        ]).

-spec supravel(string()) -> [string()].
supravel(Xs) ->
    pfad_util:min_by(
      fun erlang:length/1,
      lists:filter(fun (Yss) -> lists:all(fun up/1, Yss) end, unravels(Xs))).

-spec up(string()) -> boolean().
up([])            -> true;
up([_])           -> true; 
up([X, Y | Rest]) -> X =< Y andalso up([Y | Rest]).

-spec unravels(string()) -> [[string()]].
unravels(Xs) ->
    lists:foldr(fun (X, Xss) -> lists:flatmap(prefixes(X), Xss) end, [[]], Xs).

-spec prefixes(char()) -> fun (([string()]) -> [[string()]]).
prefixes(X) ->
    fun ([])         -> [[[X]]];
        ([Xs | Xss]) -> [[[X | Xs] | Xss]] ++ [[Xs | Yss] || Yss <- (prefixes(X))(Xss)]
    end.

-spec greedy_supravel(string()) -> [[string()]].
greedy_supravel(Xs) ->
    lists:foldr(fun insert/2, [], Xs).

-spec insert(char(), [string()]) -> [string()].
insert(X, [])                          -> [[X]];
insert(X, [Xs | Xss]) when X =< hd(Xs) -> [[X | Xs] | Xss];
insert(X, [Xs | Xss])                  -> [Xs | insert(X, Xss)].







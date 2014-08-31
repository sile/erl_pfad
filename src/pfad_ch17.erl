%% @doc Chapter17: The Knuth-Morris-Pratt algorithm
-module(pfad_ch17).

-export([
         matches/2,
         p129_matches/2,
         linear_matches/2
        ]).

-spec matches(string(), string()) -> [non_neg_integer()].
matches(Pattern, Text) ->
    EndsWith = fun (Prefix) -> lists:member(Pattern, pfad_util:tails(Prefix)) end,
    lists:map(fun erlang:length/1, lists:filter(EndsWith, pfad_util:inits(Text))).

-spec p129_matches(string(), string()) -> [non_neg_integer()].
p129_matches(Pattern, Text) ->
    Op = fun Op({Us, [X | Vs]}, X) -> {Us ++ [X], Vs};
             Op({[], _}, _)        -> {[], Pattern};
             Op({Us, _}, X)        -> Op(split(Pattern, tl(Us)), X)
         end,
    Step = fun (X, {N, {Us, Vs}}) -> {N + 1, Op({Us, Vs}, X)} end,
    [Pos || {Pos, {_, []}} <- pfad_util:scanl(Step, {0, {[], Pattern}}, Text)].

-spec split(string(), string()) -> {string(), string()}.
split(Ws, Xs) ->
    hd([{Us, lists:nthtail(length(Us), Ws)} || Us <- pfad_util:tails(Xs) ++ [""], lists:prefix(Us, Ws)]).

-spec linear_matches(string(), string()) -> [non_neg_integer()].
linear_matches(Pattern, Text) ->
    Ok   = fun (Rep)  ->
                   {node, Vs, _, _} = Rep(),
                   Vs =:= []
           end,
    Op   = fun Op(Rep, X, Root) ->
                   case Rep() of
                       null                  -> Root;
                       {node, [X | _], _, R} -> R;
                       {node, [],      L, _} -> Op(L, X, Root); % all matched
                       {node, _,       L, _} -> Op(L, X, Root)
                   end
           end,
    Null = fun () -> null end,
    Grep = fun Grep(L, [])       -> fun() -> {node, [], L, Null} end;
               Grep(L, [V | Vs]) -> fun() -> {node, [V | Vs], L, Grep(Op(L, V, Grep(Null, Pattern)), Vs)} end
           end,

    Root = Grep(Null, Pattern),

    Step = fun (X, {N, T}) -> {N + 1, Op(T, X, Root)} end,
    [Fst || {Fst, Snd} <- pfad_util:scanl(Step, {0, Root}, Text), Ok(Snd)].

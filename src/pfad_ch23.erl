%% @doc Chapter23: Inside the convex hull
-module(pfad_ch23).

-export([
         dimension/1,
         orientation/1,
         facets/1,
         inside_cs/2,
         simplexes/1,
         tuples/2,
         partition/1,

         inside_ch/2,
         inside_ch_/2,
         inside_ch__/2
        ]).

-type point() :: [integer()].
-type simplex() :: {[point()], -1 | +1}.
-type facet() :: {[point()], -1 | +1}.

-spec dimension(point()) -> integer().
dimension(Ps) -> length(Ps) - 1.

-spec orientation([point()]) -> integer().
orientation(Points) ->
    pfad_util:signum(pfad_ch22:interleaved_integer_division_det(Points)).

-spec facets(simplex()) -> [facet()].
facets({Us, B}) ->
    Ms = pfad_util:minors(Us),
    lists:zip(Ms, pfad_util:cycle([B, -B], length(Ms))).

-spec inside_cs(simplex(), point()) -> boolean().
inside_cs(Smp, P) ->
    lists:all(fun ({Us, B}) ->
                      0 =< B * orientation([P | Us])
              end,
              facets(Smp)).

-spec inside_ch([point()], point()) -> boolean().
inside_ch(Vs, P) ->
    lists:any(fun (Smp) -> inside_cs(Smp, P) end,
              simplexes(Vs)).

-spec simplexes([point()]) -> [simplex()].
simplexes(Vs) ->
    D = dimension(hd(Vs)),
    [{Us, orientation(Us)} || Us <- tuples(D + 1, Vs), orientation(Us) =/= 0].

-spec tuples(non_neg_integer(), [point()]) -> [[point()]].
tuples(0, _)        -> [];
tuples(_, [])       -> [];
tuples(1, Vs)       -> [[V] || V <- Vs];
tuples(N, [V | Vs]) ->
    [[V | Ys] || Ys <- tuples(N - 1, Vs), Ys =/= []] ++ tuples(N, Vs).

-spec inside_ch_([point()], point()) -> boolean().
inside_ch_(Vs, P) ->
    lists:any(fun (Smp) -> inside_cs(Smp, P) end, partition(ordsets:from_list(Vs))).

-spec partition(ordsets:ordset(point())) -> [simplex()].
partition(Vs) ->
    case find_simplex(Vs) of
        error     -> [];
        {ok, Smp} -> lists:foldl(fun update/2, [Smp], ordsets:subtract(Vs, vertices(Smp)))
    end.

-spec vertices(simplex()) -> ordsets:ordset(point()).
vertices({Points, _}) -> ordsets:from_list(Points).

-spec degenerate(non_neg_integer(), [point()]) -> boolean().
degenerate(K, Vs) ->
    lists:all(fun (N) -> N =:= 0 end, lists:map(fun pfad_ch22:interleaved_integer_division_det/1, submatrices(K, pfad_util:transpose(Vs)))).

-spec submatrices(non_neg_integer(), [point()]) -> [[point()]].
submatrices(K, Vs) ->
    Init = lists:droplast(Vs),
    Last = lists:last(Vs),
    [Ys ++ [Last] || Ys <- tuples(K, Init)].

-spec find_simplex(ordsets:ordset(point())) -> {ok, simplex()} | error.
find_simplex([])       -> error;
find_simplex([V | Vs]) -> search(length(V) - 1, 1, [V], Vs).

-spec search(non_neg_integer(), non_neg_integer(), [point()], [point()]) -> {ok, simplex()} | error.
search(D, K, Us, _) when K =:= D + 1 -> {ok, {Us, orientation(Us)}};
search(_, _, _, [])                  -> error;
search(D, K, Us, [V | Vs])           ->
    case degenerate(K, [V | Us]) of
        true  -> search(D, K, Us, Vs);
        false -> search(D, K + 1, [V | Us], Vs)
    end.

-spec external([simplex()]) -> [facet()].
external(Smps0) ->
    Op = fun (Smp, [])            -> [Smp];
             (Smp, [Smp_ | Smps]) ->
                 case vertices(Smp) =:= vertices(Smp_) of
                     true  -> Smps;
                     false -> [Smp, Smp_ | Smps]
                 end
         end,
    lists:foldr(Op, [], lists:sort(pfad_util:concat_map(fun facets/1, Smps0))).

-spec visible(point(), [facet()]) -> [facet()].
visible(V, Fs) ->
    [{Us, B} || {Us, B} <- Fs, B * orientation([V | Us]) < 0].

-spec new_simplex(point(), facet()) -> simplex().
new_simplex(V, {Us, B}) -> {[V | Us], B}.

-spec update(point(), [simplex()]) -> [simplex()].
update(V, Smps) ->
    Smps ++ [new_simplex(V, Facet) || Facet <- visible(V, external(Smps))].

-spec inside_ch__([point()], point()) -> boolean().
inside_ch__(Vs, P) ->
    case faces(ordsets:from_list(Vs)) of
        [] -> false;
        Fs -> lists:all(fun ({Us, B}) -> 0 =< B * orientation([P | Us]) end, Fs)
    end.

%% @equiv external(partition(Vs))
-spec faces(ordsets:ordset(point())) -> [facet()].
faces(Vs) ->
    Update =
        fun (V, Fs) ->
                NewFacet = fun ({Us, B}) -> {[V | Us], B} end,
                Fs_ = visible(V, Fs),
                ordsets:subtract(Fs, Fs_) ++ lists:map(NewFacet, external(Fs_))
        end,
    case find_simplex(Vs) of
        error     -> [];
        {ok, Smp} -> lists:foldl(Update, facets(Smp), ordsets:subtract(Vs, vertices(Smp)))
    end.

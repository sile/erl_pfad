%% @doc Chapter9: Finding celebrities
-module(pfad_ch09).

-export([
         exponential_cclique/1,
         quadratic_cclique/1,
         linear_cclique/1
        ]).

-type person() :: {Name::term(), ordsets:ordset(KnowName::term())}.
-type people() :: ordsets:ordset(person()).
-type celebrity_clique() :: people().

-spec exponential_cclique(people()) -> celebrity_clique().
exponential_cclique(People) ->
    hd(exponential_ccliques(People)).

-spec exponential_ccliques(people()) -> [celebrity_clique()].
exponential_ccliques(People) ->
    lists:filter(fun (MaybeClique) -> is_celebrity_clique(MaybeClique, People) end,
                 subseqs(People)).

-spec subseqs(people()) -> [people()].
subseqs([])       -> [[]];
subseqs([X | Xs]) ->
    Yss = subseqs(Xs),
    [[X | Ys] || Ys <- Yss] ++ Yss.

-spec is_celebrity_clique(people(), people()) -> boolean().
is_celebrity_clique(MaybeClique, People) ->
    MaybeCliqueNames = [Name || {Name, _} <- MaybeClique],
    lists:all(
      fun ({_, Knowns}) -> ordsets:is_subset(MaybeCliqueNames, Knowns) end,
      People)
        andalso
    lists:all(
      fun ({_, Knowns}) -> MaybeCliqueNames =:= Knowns end,
      MaybeClique).

-spec quadratic_cclique(people()) -> celebrity_clique().
quadratic_cclique(People) ->
    hd(lists:filter(fun (Cs) -> is_celebrity_clique(Cs, People) end, quadratic_ccliques(People))).

-spec quadratic_ccliques(people()) -> [celebrity_clique()].
quadratic_ccliques([])       -> [[]];
quadratic_ccliques([P | Ps]) ->
    Css = quadratic_ccliques(Ps),
    [[P | Cs] || Cs <- Css, is_member(P, Ps, Cs)]
        ++
    [Cs || Cs <- Css, is_nonmember(P, Cs)].

-spec is_member(person(), people(), celebrity_clique()) -> boolean().
is_member(P, Ps, Cs) ->
    lists:all(fun (X) -> know(X, P) end, Ps)
        andalso
    lists:all(fun (C) -> know(P, C) end, Cs).

-spec is_nonmember(person(), celebrity_clique()) -> boolean().
is_nonmember(P, Cs) ->
    lists:all(fun (C) -> know(P, C) andalso (not know(C, P)) end, Cs).

%% @doc Assume celebrity clique always exists.
-spec linear_cclique(people()) -> celebrity_clique().
linear_cclique(People) ->
    lists:foldr(fun op/2, [], People).

-spec op(person(), celebrity_clique()) -> celebrity_clique().
op(P, []) -> [P];
op(P, Cs) ->
    C = hd(Cs),
    case {know(P, C), know(C, P)} of
        {false, _} -> [P];
        {_, false} -> Cs;
        _          -> [P | Cs]
    end.

%% @doc Assume this function can be performed in constant time.
-spec know(person(), person()) -> boolean().
know({_, Knows}, {Name, _}) ->
    ordsets:is_element(Name, Knows).

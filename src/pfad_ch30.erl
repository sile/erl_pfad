%% @doc Chapter30: Spider spinning for dummies
-module(pfad_ch30).

-export([
         tree_ncode/1,
         tree_loopless_ncode/1,
         ncode/1,
         fig_30_5_ncode/1,
         fig_30_7_ncode/1,
         seed/1
        ]).

-type tree_nest() :: [tree_spider()].
-type tree_spider() :: {node, integer(), tree_nest()}.

-spec tree_ncode(tree_nest()) -> [integer()].
tree_ncode(Spiders) ->
    pfad_ch28:boxall(lists:map(fun tree_scode/1, Spiders)).

-spec tree_scode(tree_spider()) -> [integer()].
tree_scode({node, A, Xs}) -> [A | tree_ncode(Xs)].

-spec tree_loopless_ncode(tree_nest()) -> [integer()].
tree_loopless_ncode(Spiders) ->
    Op =
        fun Op({node, A, Xs}, {Bs, Sb}) ->
                {Cs, Sc} = lists:foldr(Op, {Sb, Bs}, Xs),
                {queue:in({node, A, Cs}, Bs), queue:in({node, A, Sb}, Sc)}
        end,
    Prolog =
        pfad_ch28:wrap_queue(pfad_util:fst(lists:foldr(Op, {queue:new(), queue:new()}, Spiders))),
    pfad_util:unfoldr(fun pfad_ch28:step/1, Prolog).

-type nest() :: [spider()].
-type spider() :: {node, integer(), [leg()]}.
-type leg() :: {dn, spider()} | {up, spider()}.

-spec ncode(nest()) -> [integer()].
ncode(Spiders) ->
    bcode([{dn, Node} || Node <- Spiders]).

-spec wcode([leg()]) -> [integer()].
wcode(Legs) ->
    coxall(lists:map(fun wc/1, Legs)).

-spec bcode([leg()]) -> [integer()].
bcode(Legs) ->
    pfad_ch28:boxall(lists:map(fun bc/1, Legs)).

-spec wc(leg()) -> [integer()].
wc({up, {node, A, Legs}}) -> wcode(Legs) ++ [A] ++ bcode(Legs);
wc({dn, {node, _, Legs}}) -> lists:reverse(wcode(Legs)).

-spec bc(leg()) -> [integer()].
bc({up, {node, _, Legs}}) -> lists:reverse(bcode(Legs));
bc({dn, {node, A, Legs}}) ->  wcode(Legs) ++ [A] ++ bcode(Legs).

-spec cox([A], [A]) -> [A] when A :: term().
cox(As, Bs) ->
    lists:reverse(pfad_ch28:box(lists:reverse(As), lists:reverse(Bs))).

-spec coxall([[A]]) -> [A] when A :: term().
coxall(Ass) ->
    lists:foldr(fun cox/2, [], Ass).

-spec fig_30_5_ncode(nest()) -> [integer()].
fig_30_5_ncode(Spiders) ->
    lists:foldr(fun fig_30_5_bop/2, [], [{dn, Node} || Node <- Spiders]).

-spec fig_30_5_bop(leg(), [integer()]) -> [integer()].
fig_30_5_bop({up, {node, _, Legs}}, Cs) ->
    Cs2 = case length(lists:foldr(fun fig_30_5_bop/2, [], Legs)) rem 2 of
              0 -> lists:reverse(Cs);
              1 -> Cs
          end,
    lists:reverse(lists:foldr(fun fig_30_5_bop/2, Cs2, Legs));
fig_30_5_bop({dn, {node, A, Legs}}, Cs) ->
    Cs2 = case length(lists:foldr(fun fig_30_5_wop/2, [], Legs)) rem 2 of
              0 -> lists:reverse(Cs);
              1 -> Cs
          end,
    lists:foldr(fun fig_30_5_wop/2, lists:reverse(Cs2), Legs)
        ++ [A] ++
    lists:foldr(fun fig_30_5_bop/2, Cs2, Legs).

-spec fig_30_5_wop(leg(), [integer()]) -> [integer()].
fig_30_5_wop({up, {node, A, Legs}}, Cs) ->
    Cs2 = case length(lists:foldr(fun fig_30_5_bop/2, [], Legs)) rem 2 of
              0 -> lists:reverse(Cs);
              1 -> Cs
          end,
    lists:foldr(fun fig_30_5_wop/2, Cs2, Legs)
        ++ [A] ++
    lists:foldr(fun fig_30_5_bop/2, lists:reverse(Cs2), Legs);
fig_30_5_wop({dn, {node, _, Legs}}, Cs) ->
    Cs2 = case length(lists:foldr(fun fig_30_5_wop/2, [], Legs)) rem 2 of
              0 -> lists:reverse(Cs);
              1 -> Cs
          end,
    lists:reverse(lists:foldr(fun fig_30_5_wop/2, Cs2, Legs)).

-type spider2() :: {node, {boolean(), boolean()}, integer(), [leg2()]}.
-type leg2() :: {dn, spider2()} | {up, spider2()}.

-spec decorate(spider()) -> spider2().
decorate({node, A, Legs}) ->
    Legs2 =
        [case Leg of
             {up, X} -> {up, decorate(X)};
             {dn, X} -> {dn, decorate(X)}
         end || Leg <- Legs],
    Op =
        fun ({up, {node, {W, B}, _, _}}, {W2, B2}) -> {W =/= B andalso W2, B andalso B2};
            ({dn, {node, {W, B}, _, _}}, {W2, B2}) -> {W andalso W2, W =/= B andalso B2}
        end,
    {node, lists:foldr(Op, {true, true}, Legs2), A, Legs2}.

-spec fig_30_7_ncode(nest()) -> [integer()].
fig_30_7_ncode(Spiders) ->
    Spiders2 =
        [{dn, decorate(Node)} || Node <- Spiders],
    Prolog =
        pfad_ch28:wrap_queue(pfad_util:fst(lists:foldr(fun bop/2, {queue:new(), queue:new()}, Spiders2))),
    pfad_util:unfoldr(fun pfad_ch28:step/1, Prolog).

-type pair(A) :: {A, A}.

-spec bop(leg2(), pair(pfad_ch28:forest(integer()))) -> pair(pfad_ch28:forest(integer())).
bop({up, {node, {_, B}, _, Legs}}, Ps) ->
    swap(lists:foldr(fun bop/2, swapif(B, Ps), Legs));
bop({dn, {node, {W, _}, A, Legs}}, Ps) ->
    cat(A, lists:foldr(fun wop/2, swapif(not W, Ps), Legs), lists:foldr(fun bop/2, swapif(W, Ps), Legs)).

-spec wop(leg2(), pair(pfad_ch28:forest(integer()))) -> pair(pfad_ch28:forest(integer())).
wop({up, {node, {_, B}, A, Legs}}, Ps) ->
    cat(A, lists:foldr(fun wop/2, swapif(B, Ps), Legs), lists:foldr(fun bop/2, swapif(not B, Ps), Legs));
wop({dn, {node, {W, _}, _, Legs}}, Ps) ->
    swap(lists:foldr(fun wop/2, swapif(W, Ps), Legs)).

-spec cat(integer(), Ps, Ps) -> Ps when Ps :: pair(pfad_ch28:forest(integer())).
cat(A, {Ws, Sw}, {Bs, Sb}) ->
    {queue:in({node, A, Bs}, Ws), queue:in({node, A, Sw}, Sb)}.

-spec swap(pair(A)) -> pair(A) when A :: term().
swap({Xs, Ys}) -> {Ys, Xs}.

-spec swapif(boolean(), pair(A)) -> pair(A) when A :: term().
swapif(true,  {Xs, Ys}) -> {Ys, Xs};
swapif(false, {Xs, Ys}) -> {Xs, Ys}.

-type bit() :: 0 | 1.
-type state() :: map(pos_integer(), bit()).

-spec seed(nest()) -> [bit()].
seed(Spiders) ->
    Spiders2 = [{dn, decorate(Node)} || Node <- Spiders],
    maps:values(pfad_util:fst(bseed(Spiders2))).

-spec bseed([leg2()]) -> {state(), state()}.
bseed(Legs) ->
    lists:foldr(fun bsp/2, {maps:new(), maps:new()}, lists:map(fun bs/1, Legs)).

-spec wseed([leg2()]) -> {state(), state()}.
wseed(Legs) ->
    lists:foldr(fun wsp/2, {maps:new(), maps:new()}, lists:map(fun ws/1, Legs)).

-spec bs(leg2()) -> {boolean(), state(), state()}.
bs({up, {node, {_, B}, A, Legs}}) ->
    {Is, Fs} = bseed(Legs),
    {B, maps:put(A, 1, Fs), maps:put(A, 1, Is)};
bs({dn, {node, {_, B}, A, Legs}}) ->
    {Is, _} = wseed(Legs),
    {_, Fs} = bseed(Legs),
    {B, maps:put(A, 0, Is), maps:put(A, 1, Fs)}.

-spec ws(leg2()) -> {boolean(), state(), state()}.
ws({up, {node, {W, _}, A, Legs}}) ->
    {Is, _} = wseed(Legs),
    {_, Fs} = bseed(Legs),
    {W, maps:put(A, 0, Is), maps:put(A, 1, Fs)};
%%    {not W, maps:put(A, 0, Is), maps:put(A, 1, Fs)};
ws({dn, {node, {W, _}, A, Legs}}) ->
    {Is, Fs} = wseed(Legs),
    {W, maps:put(A, 0, Fs), maps:put(A, 0, Is)}.

-spec bsp({boolean(), state(), state()}, {state(), state()}) -> {state(), state()}.
bsp({B, Ia, Fa}, {Ib, Fb}) ->
    {union(Ia, Ib), union(Fa, case B of true -> Fb; false -> Ib end)}.

-spec wsp({boolean(), state(), state()}, {state(), state()}) -> {state(), state()}.
wsp({W, Ia, Fa}, {Ib, Fb}) ->
    {union(Ia, case W of true -> Ib; false -> Fb end), union(Fa, Fb)}.

-spec union(map(), map()) -> map().
union(A, B) ->
    maps:merge(B, A).

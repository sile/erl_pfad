%% @doc Chapter26: The Schorr-Waite algorithm
-module(pfad_ch26).

-export([
         mark/2,
         p223_mark/2,
         p224_mark/2,
         p229_mark/2
        ]).

-type graph_node() :: pos_integer()
                    | 0. % special node (acts as a list terminator).

-type graph() :: #{
             get => fun ((graph_node()) -> {graph_node(), graph_node()}),
             set => fun ((graph_node(), left|right, graph_node()) -> graph())
           }.

-type is_reachable() :: fun ((graph_node()) -> boolean()).

-spec left(graph(), graph_node()) -> graph_node().
left(#{get := G}, N) -> element(1, G(N)).

-spec right(graph(), graph_node()) -> graph_node().
right(#{get := G}, N) -> element(2, G(N)).

-spec setl(graph(), graph_node(), graph_node()) -> graph().
setl(#{set := S}, N, L) -> S(N, left, L).

-spec setr(graph(), graph_node(), graph_node()) -> graph().
setr(#{set := S}, N, R) -> S(N, right, R).

-spec mark(graph(), graph_node()) -> {graph(), is_reachable()}.
mark(G, Root) ->
    seek0({G, const(false)}, [Root]).

-spec seek0({graph(), is_reachable()}, [graph_node()]) -> {graph(), is_reachable()}.
seek0({G, M}, [])       -> {G, M};
seek0({G, M}, [X | Xs]) ->
    case M(X) of
        false -> seek0({G, set(M, X)}, [left(G, X), right(G, X) | Xs]);
        true  -> seek0({G, M}, Xs)
    end.

-spec const(term()) -> fun ((any()) -> term()).
const(X) ->
    fun (_) -> X end.

-spec set(is_reachable(), graph_node()) -> is_reachable().
set(F, X) ->
    fun (Y) -> Y =:= X orelse F(Y) end.

-spec unset(is_reachable(), graph_node()) -> is_reachable().
unset(F, X) ->
    fun (Y) -> Y =/= X andalso F(Y) end.


-spec p223_mark(graph(), graph_node()) -> {graph(), is_reachable()}.
p223_mark(G, Root) ->
    seek1({G, const(false)}, Root, []).

-spec seek1({graph(), is_reachable()}, graph_node(), [graph_node()]) -> {graph(), is_reachable()}.
seek1({G, M}, X, Xs) ->
    case {M(X), Xs} of
        {false, _} -> seek1({G, set(M, X)}, left(G, X), [X | Xs]);
        {_, []}    -> {G, M};
        _          -> seek1({G, M}, right(G, hd(Xs)), tl(Xs))
    end.

-spec p224_mark(graph(), graph_node()) -> {graph(), is_reachable()}.
p224_mark(G, Root) ->
    seek2({G, const(false)}, const(false), Root, []).

-spec seek2({graph(), is_reachable()}, is_reachable(), graph_node(), [graph_node()]) -> {graph(), is_reachable()}.
seek2({G, M}, P, X, Xs) ->
    case M(X) of
        false -> seek2({G, set(M, X)}, set(P, X), left(G, X), [X | Xs]);
        true  -> find2({G, M}, P, Xs)
    end.

-spec find2({graph(), is_reachable()}, is_reachable(), [graph_node()]) -> {graph(), is_reachable()}.
find2({G, M}, _, [])       -> {G, M};
find2({G, M}, P, [Y | Ys]) ->
    case P(Y) of
        false -> find2({G, M}, P, Ys);
        true  -> seek2({G, M}, unset(P, Y), right(G, Y), [Y | Ys])
    end.

%% -spec stack(graph(), is_reachable(), graph_node()) -> [graph_node()].
%% stack(_, _, 0) -> [];
%% stack(G, P, X) ->
%%     case P(X) of
%%         true  -> [X | stack(G, P, left(G, X))];
%%         false -> [X | stack(G, P, right(G, X))]
%%     end.

%% -spec restore(graph(), is_reachable(), graph_node(), [graph_node()]) -> graph().
%% restore(G, _, _, [])       -> G;
%% restore(G, P, X, [Y | Ys]) ->
%%     case P(Y) of
%%         true  -> restore(setl(G, Y, X), P, Y, Ys);
%%         false -> restore(setr(G, Y, X), P, Y, Ys)
%%     end.

-spec p229_mark(graph(), graph_node()) -> {graph(), is_reachable()}.
p229_mark(G, Root) ->
    seek3({G, const(false)}, const(false), Root, 0).

-spec seek3({graph(), is_reachable()}, is_reachable(), graph_node(), graph_node()) -> {graph(), is_reachable()}.
seek3({G, M}, P, X, Y) ->
    case M(X) of
        false -> seek3({setl(G, X, Y), set(M, X)}, set(P, X), left(G, X), X);
        true  -> find3({G, M}, P, X, Y)
    end.

-spec find3({graph(), is_reachable()}, is_reachable(), graph_node(), graph_node()) -> {graph(), is_reachable()}.
find3({G, M}, _, _, 0) -> {G, M};
find3({G, M}, P, X, Y) ->
    Swing =
        fun (G1, Y1, X1) -> setr(setl(G1, Y1, X1), Y1, left(G1, Y1)) end,
    case P(Y) of
        true  -> seek3({Swing(G, Y, X), M}, unset(P, Y), right(G, Y), Y);
        false -> find3({setr(G, Y, X), M}, P, Y, right(G, Y))
    end.

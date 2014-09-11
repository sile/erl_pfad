%% @doc Chapter28: Loopless functional algorithms
-module(pfad_ch28).

-export([
         box/2,
         rbox/2,
         boxall/1,
         loopless_boxall_op1/1,
         loopless_boxall_op2/1
        ]).

-export([
         cons_queue/2,
         wrap_queue/1
        ]).

%%```
%% > pfad_ch28:box([3,4], [0,1,2]).
%% [0,1,2,3,2,1,0,4,0,1,2]
%%'''
-spec box([A], [A]) -> [A] when A :: term().
box([], Ys)       -> Ys;
box([X | Xs], Ys) -> Ys ++ [X] ++ box(Xs, lists:reverse(Ys)).

-spec rbox([A], [A]) -> [A] when A :: term().
rbox([], Sy)       -> Sy;
rbox([X | Xs], Sy) -> rbox(Xs, lists:reverse(Sy)) ++ [X] ++ Sy.

-spec boxall([[A]]) -> [A] when A :: term().
boxall(ListOfList) ->
    lists:foldr(fun box/2, [], ListOfList).

-type forest(A) :: queue:queue(rose(A)).
-type rose(A) :: {node, A, forest(A)}.

-spec loopless_boxall_op1([[A]]) -> [A] when A :: term().
loopless_boxall_op1(ListOfList) ->
    pfad_util:unfoldr(fun step/1, wrap_queue(pfad_util:fst(lists:foldr(fun op1/2, {queue:new(), queue:new()}, ListOfList)))).

-spec loopless_boxall_op2([[A]]) -> [A] when A :: term().
loopless_boxall_op2(ListOfList) ->
    pfad_util:unfoldr(fun step/1, wrap_queue(pfad_util:fst(lists:foldr(fun op2/2, {queue:new(), queue:new()}, ListOfList)))).


-spec op1([A], {forest(A), forest(A)}) -> {forest(A), forest(A)} when A :: term().
op1([],       {Ys, Sy}) -> {Ys, Sy};
op1([X | Xs], {Ys, Sy}) ->
    {Zs, Sz} = op1(Xs, {Sy, Ys}),
    {queue:in({node, X, Zs}, Ys), queue:in({node, X, Sy}, Sz)}.

-spec op2([A], {forest(A), forest(A)}) -> {forest(A), forest(A)} when A :: term().
op2(Xs, {Ys, Sy}) ->
    case length(Xs) rem 2 of
        0 -> {mix(Xs, {Ys, Sy}), mix(lists:reverse(Xs), {Sy, Ys})};
        1 -> {mix(Xs, {Ys, Sy}), mix(lists:reverse(Xs), {Ys, Sy})}
    end.

-spec mix([A], {forest(A), forest(A)}) -> forest(A).
mix([], {Ys, _})        -> Ys;
mix([X | Xs], {Ys, Sy}) -> queue:in({node, X, mix(Xs, {Sy, Ys})}, Ys).

-spec wrap_queue(queue:queue(A)) -> [queue:queue(A)] when A :: term().
wrap_queue(Xs) -> cons_queue(Xs, []).

-spec cons_queue(queue:queue(A), [queue:queue(A)]) -> [queue:queue(A)] when A :: term().
cons_queue(Xs, Xss) ->
    case queue:is_empty(Xs) of
        true  -> Xss;
        false -> [Xs | Xss]
    end.

-spec step([forest(A)]) -> {ok, A, [forest(A)]} | error when A :: term().
step([])         -> error;
step([Zs | Zss]) ->
    {{value, {node, X, Xs}}, Ys} = queue:out(Zs),
    {ok, X, cons_queue(Xs, cons_queue(Ys, Zss))}.

%% @doc Chapter29: The Johnson-Trotter algorithm
-module(pfad_ch29).

-export([
         jcode/1,
         boxall_jcode/1,
         loopless_jcode/1
        ]).

-spec jcode(integer()) -> [integer()].
jcode(1) -> [];
jcode(N) -> pfad_ch28:box(bump_by(1, jcode(N - 1)), lists:seq(N - 1, 1, -1)).

-spec bump_by(integer(), [integer()]) -> [integer()].
bump_by(_, [])          -> [];
bump_by(K, [A])         -> [A + K];
bump_by(K, [A, B | As]) -> [A + K, B | bump_by(K, As)].

-spec bump_dn({integer(), integer()}) -> [integer()].
bump_dn({K, N}) ->
    bump_by(K, lists:seq(N - 1, 1, -1)).

-spec add_pair({integer(), integer()}, [{integer(), integer()}]) -> [{integer(), integer()}].
add_pair({_, 1}, Ps) -> Ps;
add_pair({K, N}, Ps) ->
    K2 = case N rem 2 of
             0 -> 1;
             1 -> K + 1
         end,
    add_pair({K2, N - 1}, [{K, N} | Ps]).

-spec pairs(integer()) -> [{integer(), integer()}].
pairs(N) ->
    add_pair({0, N}, []).

-spec boxall_jcode(integer()) -> [integer()].
boxall_jcode(N) ->
    pfad_ch28:boxall(lists:map(fun bump_dn/1, pairs(N))).

-type forest(A) :: queue:queue(rose(A)).
-type rose(A) :: {node, A, {forest(A), forest(A)}}.

-type state() :: {integer(), integer(), integer(), integer(), integer()}.
-type pair(A) :: {A, A}.

-spec loopless_jcode(integer()) -> [integer()].
loopless_jcode(N) ->
    Prolog = pfad_ch28:wrap_queue(pfad_util:fst(lists:foldr(fun op/2, {queue:new(), queue:new()}, pairs(N)))),
    pfad_util:unfoldr(fun step/1, Prolog).

-spec step([forest({integer(), state()})]) -> {ok, integer(), [forest({integer(), state()})]} | error.
step([])         -> error;
step([Zs | Zss]) ->
    {{value, {node, {X, Q}, {Ys, Sy}}}, Zs2} = queue:out(Zs),
    {ok, X, pfad_ch28:cons_queue(mix(Q, {Sy, Ys}), pfad_ch28:cons_queue(Zs2, Zss))}.

-spec mix(state(), pair(forest({integer(), state()}))) -> forest({integer(), state()}).
mix({I, J, K, M, N}, {Ys, Sy}) ->
    case I * (N - M) < 0 of
        true  -> Ys;
        false -> queue:in({node, {M + J, {I, K - J, K, M + I, N}}, {Ys, Sy}}, Ys)
    end.

-spec op({integer(), integer()}, pair(forest({integer(), state()}))) -> pair(forest({integer(), state()})).
op({K, N}, {Ys, Sy}) ->
    case N rem 2 of
        1 ->
            {mix({-1, K, K, N - 1, 1}, {Ys, Sy}),
             mix({1, 0, K, 1, N -1}, {Sy, Ys})};
        0 ->
            {mix({-1, K, K, N - 1, 1}, {Ys, Sy}),
             mix({1, K, K, 1, N - 1}, {Ys, Sy})}
    end.

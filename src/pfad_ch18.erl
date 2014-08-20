%% @doc Chapter16: The Boyer-Moore algorithm
-module(pfad_ch18).

-export([
         bfsearch/3,
         p138_bfsearch/4,

         psearch/3,

         bfsolve/1,
         psolve/1
        ]).

%% for General Puzzle Problem
-type state()    :: term().
-type move()     :: term().
-type path()     :: {[move()], state()}.
-type frontier() :: [path()].

-type solved_fun() :: fun ((state()) -> boolean()).
-type moves_fun()  :: fun ((state()) -> [move()]).
-type move_fun()   :: fun ((state(), move()) -> state()).

-type search_funs() :: #{solved => solved_fun(),
                         moves  => moves_fun(),
                         move   => move_fun()}.

-type plan()       :: [move()].
-type a_path()     :: {[move()], state(), plan()}.
-type a_frontier() :: [a_path()].

-type premoves_fun() :: fun ((state(), move()) -> [plan()]).
-type goalmove_fun() :: fun ((state(), move()) -> plan()).

-type expand_fun() :: fun ((state(), move()) -> [move()]).

-type psearch_funs() :: #{solved   => solved_fun(),
                          moves    => moves_fun(),
                          move     => move_fun(),
                          premoves => premoves_fun(),
                          goalmove => goalmove_fun(),

                          expand   => expand_fun()}.

-spec bfsearch(search_funs(), [state()], frontier()) -> {ok, [move()]} | error.
bfsearch(_, _, []) ->
    error;
bfsearch(Funs = #{solved := Solved}, Qs, [P = {Ms, Q} | Ps]) ->
    case {Solved(Q), lists:member(Q, Qs)} of
        {true, _} -> {ok, Ms};
        {_, true} -> bfsearch(Funs, Qs, Ps);
        _         -> bfsearch(Funs, [Q | Qs], Ps ++ succs(Funs, P))
    end.

-spec p138_bfsearch(search_funs(), [state()], [frontier()], frontier()) -> {ok, [move()]} | error.
p138_bfsearch(_, _, [], []) ->
    error;
p138_bfsearch(Funs, Qs, Pss, []) ->
    p138_bfsearch(Funs, Qs, [], lists:append(lists:reverse(Pss)));
p138_bfsearch(Funs = #{solved := Solved}, Qs, Pss, [P = {Ms, Q} | Ps]) ->
    case {Solved(Q), lists:member(Q, Qs)} of
        {true, _} -> {ok, Ms};
        {_, true} -> p138_bfsearch(Funs, Qs, Pss, Ps);
        _         -> p138_bfsearch(Funs, [Q | Qs], [succs(Funs, P) | Pss], Ps)
    end.

-spec succs(search_funs(), path()) -> [path()].
succs(#{moves := Moves, move := Move}, {Ms, Q}) ->
    [{Ms ++ [M], Move(Q, M)} || M <- Moves(Q)].

-spec psearch(psearch_funs(), [state()], a_frontier()) -> {ok, [move()]} | error.
psearch(_, _, []) ->
    error;
psearch(Funs = #{solved := Solved}, Qs, [P = {Ms, Q, _} | Ps]) ->
    case {Solved(Q), lists:member(Q, Qs)} of
        {true, _} -> {ok, Ms};
        {_, true} -> psearch(Funs, Qs, Ps);
        _         -> psearch(Funs, [Q | Qs], asuccs(Funs, P) ++ Ps ++ bsuccs(Funs, P))
    end.

-spec asuccs(psearch_funs(), a_path()) -> [a_path()].
asuccs(Funs = #{move := Move}, {Ms, Q, Plan0}) ->
    [{Ms ++ [M], Move(Q, M), Plan1} || [M | Plan1] <- newplans(Funs, Q, Plan0)].

-spec bsuccs(psearch_funs(), a_path()) -> [a_path()].
bsuccs(#{moves := Moves, move := Move, goalmoves := GoalMoves}, {Ms, Q0, _}) ->
    [begin
         Q1 = Move(Q0, M),
         {Ms ++ [M], Q1, GoalMoves(Q1)}
     end || M <- Moves(Q0)].

%% p.139
%% -spec newplans(psearch_funs(), state(), plan()) -> [plan()].
%% newplans(#{moves := Moves, premoves := PreMoves}, Q, Ms0) ->
%%     Qms = Moves(Q),
%%     MkPlans =
%%         fun MkPlans([])           -> [];
%%             MkPlans([M | _] = Ms) ->
%%                 case lists:member(M, Qms) of
%%                     true  -> [Ms];
%%                     false ->
%%                         lists:append(
%%                           [MkPlans(Pms ++ Ms) ||
%%                               Pms <- PreMoves(Q, M),
%%                               lists:all(fun (X) -> not lists:member(X, Ms) end, Pms)])
%%                 end
%%         end,
%%     MkPlans(Ms0).


%% for Rush Hour Problem
-type rh_cell()    :: integer().
-type rh_grid()    :: [{rh_cell(), rh_cell()}].
-type rh_vehicle() :: integer().
-type rh_move()    :: {rh_vehicle(), rh_cell()}.
%% -type rh_state()   :: rh_grid().
-type rh_plan()    :: [rh_move()].

-spec occupied(rh_grid()) -> [rh_cell()].
occupied(Grid) ->
    FillCells =
        fun ({R, F}) when R > F - 7 -> lists:seq(R, F);   % horizontal
            ({R, F})                -> lists:seq(R, F, 7) % vertical
        end,
    lists:foldr(fun (Cell, Acc) -> lists:umerge(FillCells(Cell), Acc) end, [], Grid).

-spec freecells(rh_grid()) -> [rh_cell()].
freecells(Grid) ->
    AllCells = [C || C <- lists:seq(1, 41), C rem 7 =/= 0],
    AllCells -- occupied(Grid).

-spec moves(rh_grid()) -> [rh_move()].
moves(G) ->
    Adjs =
        fun ({R, F}) when R > F - 7 -> [F + 1, R - 1]; % horizontal
            ({R, F})                -> [F + 7, R - 7]  % vertical
        end,
    Fs = freecells(G),
    [{V, C} || {V, I} <- lists:zip(lists:seq(0, length(G) - 1), G),
               C      <- Adjs(I),
               lists:member(C, Fs)].

-spec move(rh_grid(), rh_move()) -> rh_grid().
move(G0, {V, C}) ->
    Adjust =
        fun ({R, F}) when R > F - 7, C > F -> {R + 1, C};
            ({R, F}) when R > F - 7        -> {C, F - 1};
            ({R, F}) when            C < R -> {C, F - 7};
            ({R, _})                       -> {R + 7, C}
        end,
    {G1, [I | G2]} = lists:split(V, G0),
    G1 ++ [Adjust(I) | G2].

-spec solved(rh_grid()) -> boolean().
solved([{_, Cell} | _]) -> Cell =:= 20.

-spec bfsolve(rh_grid()) -> {ok, [rh_move()]} | error.
bfsolve(G) ->
    Funs = #{solved => fun solved/1,
             moves  => fun moves/1,
             move   => fun move/2},
    p138_bfsearch(Funs, [], [], [{[], G}]).

-spec goalmoves(rh_grid()) -> rh_plan().
goalmoves([{_, C0} | _]) ->
    [{0, C1} || C1 <- lists:seq(C0 + 1, 20)].

-spec blocker(rh_grid(), rh_cell()) -> {rh_vehicle(), {rh_cell(), rh_cell()}}.
blocker(G, C) ->
    Covers =
        fun ({R, F}) ->
                (R =< C andalso
                 C =< F andalso
                 (R > F - 7 orelse (C - R) rem 7 =:= 0))
        end,
    Search =
        fun Search([{V, I} | Vis]) ->
                case Covers(I) of
                    true  -> {V, I};
                    false -> Search(Vis)
                end
        end,
    Search(lists:zip(lists:seq(0, length(G) - 1), G)).

-define(WHEN(Exp, Value),
        case Exp of
            false -> [];
            true  -> [Value]
        end).

-spec freeingmoves(rh_cell(), {rh_vehicle(), {rh_cell(), rh_cell()}}) ->[[rh_move()]].
freeingmoves(C, {V, {R, F}}) ->
    K = F - F rem 7,
    M = F - R + 7,
    N = F - R + 1,
    case R > F - 7 of
        true ->
            ?WHEN(C + N < K + 7, [{V, J} || J <- lists:seq(F + 1, C + N)]) ++
            ?WHEN(C - N > K,     [{V, J} || J <- lists:seq(R - 1, C - N, -1)]);
        false ->
            ?WHEN(C - M > 0,  [{V, J} || J <- lists:seq(R - 7, C - M, -7)]) ++
            ?WHEN(C + M < 42, [{V, J} || J <- lists:seq(F + 7, C + M,  7)])
    end.

-spec premoves(rh_grid(),rh_move()) -> [[rh_move()]].
premoves(G, {_, C}) ->
    freeingmoves(C, blocker(G, C)).

%% p.144
-spec newplans(psearch_funs(), rh_grid(), rh_plan()) -> [rh_plan()].
newplans(_, _, []) ->
    [];
newplans(#{moves := Moves, premoves := PreMoves, expand := Expand}, G, [M0 | Ms0]) ->
    Gms = Moves(G),
    MkPlans =
        fun MkPlans([M | _] = Ms) ->
                case lists:member(M, Gms) of
                    true  -> [Ms];
                    false ->
                        lists:append(
                          [MkPlans(Pms ++ Ms) ||
                              Pms <- PreMoves(G, M),
                              lists:all(fun (X) -> not lists:member(X, Ms) end, Pms)])
                end
        end,
    MkPlans(Expand(G, M0) ++ Ms0).

-spec expand(rh_grid(), rh_move()) -> [rh_move()].
expand(G, {V, C}) ->
    {R, F} = lists:nth(V + 1, G),
    case {R > F - 7, C > F} of
        {true, true}   -> [{V, P} || P <- lists:seq(F + 1, C)];
        {true, false}  -> [{V, P} || P <- lists:seq(R - 1, C, -1)];
        {false, true}  -> [{V, P} || P <- lists:seq(F + 7, C, 7)];
        {false, false} -> [{V, P} || P <- lists:seq(F - 7, C, -7)]
    end.

-spec psolve(rh_grid()) -> {ok, [rh_move()]} | error.
psolve(G) ->
    Funs = #{solved    => fun solved/1,
             moves     => fun moves/1,
             move      => fun move/2,
             premoves  => fun premoves/2,
             goalmoves => fun goalmoves/1,
             expand    => fun expand/2},
    psearch(Funs, [], [{[], G, goalmoves(G)}]).

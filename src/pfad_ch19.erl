%% @doc Chapter16: The Boyer-Moore algorithm
-module(pfad_ch19).

-export([
         solve/1,
         p153_solve/1,
         p155_solve/1
        ]).

-type matrix(A) :: [row(A)].
-type row(A) :: [A].

-type grid() :: matrix(digit() | blank()).
-type digit() :: 1..9.
-type blank() :: 0.

-type choices() :: [digit()].

-spec digits() -> [digit()].
digits() ->
    lists:seq(1,9).

-spec blank(blank()) -> true;
           (digit()) -> false.
blank(0) -> true;
blank(_) -> false.

%% ```
%% > Grid =
%%   [
%%    [0,0,4,0,0,5,7,0,0],
%%    [0,0,0,0,0,9,4,0,0],
%%    [3,6,0,0,0,0,0,0,8],
%%    [7,2,0,0,6,0,0,0,0],
%%    [0,0,0,4,0,2,0,0,0],
%%    [0,0,0,0,8,0,0,9,3],
%%    [4,0,0,0,0,0,0,5,6],
%%    [0,0,5,3,0,0,0,0,0],
%%    [0,0,6,1,0,0,9,0,0]
%%   ].
%% pfad_ch19:solve(Grid).
%% '''
-spec solve(grid()) -> [grid()].
solve(Grid) ->
    lists:filter(fun valid/1, expand(choices(Grid))).

-spec choices(grid()) -> matrix(choices()).
choices(Grid) ->
    Choice =
        fun (D) ->
                case blank(D) of
                    true  -> digits();
                    false -> [D]
                end
        end,
    [[Choice(Cell) || Cell <- Row] || Row <- Grid].

-spec expand(matrix(choices())) -> [grid()].
expand(ChoicesMatrix) ->
    Cp = fun Cp([])         -> [[]];
             Cp([Xs | Xss]) ->
                 [[X | Ys] || X <- Xs, Ys <- Cp(Xss)]
         end,
    Cp([Cp(Row) || Row <- ChoicesMatrix]).

-spec valid(grid()) -> boolean().
valid(G) ->
    (lists:all(fun nodups/1, rows(G)) andalso
     lists:all(fun nodups/1, cols(G)) andalso
     lists:all(fun nodups/1, boxs(G))).

-spec nodups(list()) -> boolean().
nodups([])       -> true;
nodups([X | Xs]) -> not lists:member(X, Xs) andalso nodups(Xs).

-spec rows(matrix(A)) -> matrix(A) when A :: term().
rows(M) -> M.

-spec cols(matrix(A)) -> matrix(A) when A :: term().
cols([Xs])       -> [[X] || X <- Xs];
cols([Xs | Xss]) -> lists:zipwith(fun pfad_util:cons/2, Xs, cols(Xss)).

-spec boxs(matrix(A)) -> matrix(A)  when A :: term().
boxs(Matrix) ->
    lists:map(fun ungroup/1, ungroup(lists:map(fun cols/1, group(lists:map(fun group/1, Matrix))))).

-spec group([A]) -> [[A]] when A :: term().
group([]) -> [];
group(Xs) -> [pfad_util:take(3, Xs) | group(pfad_util:drop(3, Xs))].

-spec ungroup([[A]]) -> [A] when A :: term().
ungroup(Xss) -> lists:append(Xss).

-spec prune(matrix(choices())) -> matrix(choices()).
prune(Matrix) ->
    prune_by(fun boxs/1, prune_by(fun cols/1, prune_by(fun rows/1, Matrix))).

-spec prune_row(row(choices())) -> row(choices()).
prune_row(Row) ->
    Remove = fun (_, [D]) -> [D];
                 (Xs, Ds) -> Ds -- Xs
             end,
    Fixed = [D || [D] <- Row],
    [Remove(Fixed, Ds) || Ds <- Row].

-spec prune_by(Fun, Matrix) -> Matrix when
      Fun    :: fun ((Matrix) -> Matrix),
      Matrix :: matrix(choices()).
prune_by(Fun, Matrix) ->
    Fun(lists:map(fun prune_row/1, Fun(Matrix))).

-spec p153_solve(grid()) -> [grid()].
p153_solve(Grid) ->
    lists:filter(fun valid/1, expand(prune(choices(Grid)))).

-spec expand1(matrix(choices())) -> [matrix(choices())].
expand1(Rows) ->
    N = lists:min([length(Cs) || Cs <- lists:append(Rows), length(Cs) =/= 1]), % maybe `length(Cs) == 0'
    Smallest = fun (Cs) -> length(Cs) =:= N end,
    {Rows1, [Row | Rows2]} = break(fun (R) -> lists:any(Smallest, R) end, Rows),
    {Row1, [Cs | Row2]} = break(Smallest, Row),
    [Rows1 ++ [Row1 ++ [[C]] ++ Row2] ++ Rows2 || C <- Cs].

-spec break(Fun, list()) -> {list(), list()} when
      Fun :: fun ((term()) -> boolean()).
break(Fun, List) ->
    lists:splitwith(fun (X) -> not Fun(X) end, List).

-spec complete(matrix(choices())) -> boolean().
complete(ChoicesMatrix) ->
    Single = fun ([_]) -> true; (_) -> false end,
    lists:all(fun (Row) -> lists:all(Single, Row) end, ChoicesMatrix).

-spec safe(matrix(choices())) -> boolean().
safe(ChoicesMatrix) ->
    Ok = fun (Row) -> nodups([D || [D] <- Row]) end,
    (lists:all(Ok, rows(ChoicesMatrix)) andalso
     lists:all(Ok, cols(ChoicesMatrix)) andalso
     lists:all(Ok, boxs(ChoicesMatrix))).

-spec p155_solve(grid()) -> [grid()].
p155_solve(Grid) ->
    search(choices(Grid)).

-spec search(matrix(choices())) -> [grid()].
search(M) ->
    case not safe(M) of
        true  -> [];
        false ->
            M2 = prune(M),
            case complete(M2) of
                true  -> [[[D || [D] <- Row] || Row <- M2]];
                false -> lists:append(lists:map(fun search/1, expand1(M2)))
            end
    end.

%% @doc Chapter22: Three ways of computing determinants
-module(pfad_ch22).

-export([
         school_book_det/1,
         rational_division_det/1,
         integer_division_det/1,
         interleaved_integer_division_det/1,
         no_division_det/1
        ]).

-spec school_book_det([[integer()]]) -> integer().
school_book_det([[X]]) -> X;
school_book_det(Xss)   ->
    Col1 = lists:map(fun erlang:hd/1, Xss),
    Cols = lists:map(fun erlang:tl/1, Xss),
    pfad_util:foldr1(fun erlang:'-'/2,
                     lists:zipwith(fun erlang:'*'/2,
                                   Col1,
                                   lists:map(fun school_book_det/1, pfad_util:minors(Cols)))).

-spec rational_division_det([[number()]]) -> number().
rational_division_det([[X]]) -> X;
rational_division_det(Xss)   ->
    Reduce =
        fun ([X | Xs], Yss) ->
                [begin
                     D = Y / X,
                     lists:zipwith(fun (A, B) -> B - D * A end, Xs, Ys)
                 end || [Y | Ys] <- Yss]
        end,
    case lists:splitwith(fun ([N | _]) -> N == 0 end, Xss) of
        {_, []}           -> 0;
        {Yss, [Zs | Zss]} ->
            X = hd(Zs) * rational_division_det(Reduce(Zs, Yss ++ Zss)),
            case length(Yss) rem 2 of
                0 -> +X;
                1 -> -X
            end
    end.

-spec integer_division_det([[integer()]]) -> integer().
integer_division_det([[X]]) -> X;
integer_division_det(Xss)   ->
    Condense =
        fun (Ass) ->
                Pair = fun ([B | Bs]) -> [{B, C} || C <- Bs] end,
                Det  = fun ({{A, B}, {C, D}}) -> A * D - B * C end,
                [lists:map(Det, Pair(lists:zip(As, Bs))) || {As, Bs} <- Pair(Ass)]
        end,
    case lists:splitwith(fun ([N | _]) -> N == 0 end, Xss) of
        {_, []}           -> 0;
        {Yss, [Zs | Zss]} ->
            X = integer_division_det(Condense([Zs | Yss ++ Zss])),
            D = trunc(math:pow(hd(Zs), length(Xss) - 2)),
            Y = X div D,
            case length(Yss) rem 2 of
                0 -> +Y;
                1 -> -Y
            end
    end.

-spec interleaved_integer_division_det([[integer()]]) -> integer().
interleaved_integer_division_det(Xss) ->
    interleaved_integer_division_det(1, Xss).

-spec interleaved_integer_division_det(integer(), [[integer()]]) -> integer().
interleaved_integer_division_det(_, [[X]]) -> X;
interleaved_integer_division_det(K, Xss)   ->
    CondenseAndDivide =
        fun (Ass) ->
                Pair = fun ([B | Bs]) -> [{B, C} || C <- Bs] end,
                Det  = fun ({{A, B}, {C, D}}) -> (A * D - B * C) div K end,
                [lists:map(Det, Pair(lists:zip(As, Bs))) || {As, Bs} <- Pair(Ass)]
        end,
    case lists:splitwith(fun ([N | _]) -> N == 0 end, Xss) of
        {_, []}           -> 0;
        {Yss, [Zs | Zss]} ->
            X = interleaved_integer_division_det(hd(Zs), CondenseAndDivide([Zs | Yss ++ Zss])),
            case length(Yss) rem 2 of
                0 -> +X;
                1 -> -X
            end
    end.

-spec no_division_det([[integer()]]) -> integer().
no_division_det(Ass) ->
    Upper = fun (Css) ->
                    lists:zipwith(fun (I, Cs) -> lists:nthtail(I, Cs) end,
                                  lists:seq(0, length(Css) - 1),
                                  Css)
            end,

    N = length(Ass),
    Ass2 = case N rem 2 of
               1 -> Upper(Ass);
               0 -> [lists:map(fun erlang:'-'/1, As) || As <- Upper(Ass)]
           end,
    Bss = lists:foldl(
            fun (Ass3, AccAss) -> trimult(mut(AccAss), Ass3) end,
            Ass2,
            lists:duplicate(N - 1, Ass)),
    hd(hd(Bss)).

-spec mut([[integer()]]) -> [[integer()]].
mut(Xss) ->
    Ys = lists:map(fun erlang:'-'/1, tl(pfad_util:scanr(fun erlang:'+'/2, 0, lists:map(fun erlang:hd/1, Xss)))),
    lists:zipwith(fun pfad_util:cons/2, Ys, lists:map(fun erlang:tl/1, Xss)).

-spec trimult([[integer()]], [[integer()]]) -> [[integer()]].
trimult(Xss, Yss) ->
    Dp = fun (Xs, Ys) -> lists:sum(lists:zipwith(fun erlang:'*'/2, Xs, Ys)) end,
    lists:zipwith(fun (As, Bss) -> [Dp(As, Bs) || Bs <- Bss] end,
                  Xss,
                  submats(pfad_util:transpose(Yss))).

-spec submats([[A]]) -> [[[A]]] when A :: term().
submats([[X]]) -> [[[X]]];
submats(Xss)   ->
    [Xss | submats(lists:map(fun erlang:tl/1, tl(Xss)))].

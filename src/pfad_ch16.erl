%% @doc Chapter16: The Boyer-Moore algorithm
-module(pfad_ch16).

-export([
         matches/2,
         p120_matches/2,
         p121_matches/2,
         p122_matches/2,
         linear_matches/2
        ]).

-spec matches([term()], [term()]) -> [non_neg_integer()].
matches(Pattern, Text) ->
    EndsWith = fun (Prefix) -> lists:suffix(Pattern, Prefix) end,
    lists:map(fun erlang:length/1, lists:filter(EndsWith, pfad_util:inits(Text))).

-spec p120_matches([term()], [term()]) -> [non_neg_integer()].
p120_matches(Pattern, Text) ->
    Rpattern = lists:reverse(Pattern),
    [EndPos || {EndPos, Rprefix} <- pfad_util:scanl(fun step/2, {0, []}, Text), lists:prefix(Rpattern, Rprefix)].

-spec step(term(), {non_neg_integer(), term()}) -> {non_neg_integer(), term()}.
step(X, {N, Sx}) -> {N + 1, [X | Sx]}.

-spec p121_matches([term()], [term()]) -> [non_neg_integer()].
p121_matches(Pattern, Text) ->
    Rpattern = lists:reverse(Pattern),
    M = length(Pattern),

    Test = fun ({[], Acc})                    -> {false, lists:reverse(Acc)};
               ({[{N, Rprefix} | Rest], Acc}) ->
                   I = pfad_ch15:llcp(Rpattern, Rprefix),
                   K = shift(Rpattern, I),
                   Next = drop(K - 1, Rest),
                   case I =:= M of
                       true  -> {true, {Next, [N | Acc]}};
                       false -> {true, {Next, Acc}}
                   end
           end,
    pfad_util:while(Test, {pfad_util:scanl(fun step/2, {0, []}, Text), []}).

-spec shift([term()], non_neg_integer()) -> pos_integer().
shift(Sw, I) ->
    M = length(Sw),
    hd([K || K <- lists:seq(1, M),
             pfad_ch15:llcp(Sw, drop(K, Sw)) =:= min(I, M - K)]).

-spec drop(non_neg_integer(), list()) -> list().
drop(_, [])   -> [];
drop(0, List) -> List;
drop(N, List) -> drop(N - 1, tl(List)).

-spec take(non_neg_integer(), list()) -> list().
take(_, [])      -> [];
take(0, _)       -> [];
take(N, [H | T]) -> [H | take(N - 1, T)].

-spec p122_matches([term()], [term()]) -> [non_neg_integer()].
p122_matches(Pattern, Text) ->
    Rpattern = lists:reverse(Pattern),
    M = length(Pattern),

    Test = fun ({_J, [], Acc})                   -> {false, lists:reverse(Acc)};
               ({J, [{N, Rprefix} | Rest], Acc}) ->
                   I = case pfad_ch15:llcp(Rpattern, take(J, Rprefix)) of
                           J  -> M;
                           I0 -> I0
                       end,
                   K = shift(Rpattern, I),
                   Next = drop(K - 1, Rest),
                   case {I =:= M, M - K =< I} of
                       {true, _} -> {true, {K, Next, [N | Acc]}};
                       {_, true} -> {true, {K, Next, Acc}};
                       _         -> {true, {M, Next, Acc}}
                   end
           end,
    pfad_util:while(Test, {M, pfad_util:scanl(fun step/2, {0, []}, Text), []}).

-spec linear_matches([term()], [term()]) -> [non_neg_integer()].
linear_matches(Pattern, Text) ->
    Rpattern = lists:reverse(Pattern),
    M = length(Pattern),
    ShiftTable = make_shift_table(Rpattern),

    Test = fun ({_J, [], Acc})                   -> {false, lists:reverse(Acc)};
               ({J, [{N, Rprefix} | Rest], Acc}) ->
                   I = case pfad_ch15:llcp(Rpattern, take(J, Rprefix)) of
                           J  -> M;
                           I0 -> I0
                       end,
                   K = element(I + 1, ShiftTable),
                   Next = drop(K - 1, Rest),
                   case {I =:= M, M - K =< I} of
                       {true, _} -> {true, {K, Next, [N | Acc]}};
                       {_, true} -> {true, {K, Next, Acc}};
                       _         -> {true, {M, Next, Acc}}
                   end
           end,
    pfad_util:while(Test, {M, pfad_util:scanl(fun step/2, {0, []}, Text), []}).

-spec make_shift_table([term()]) -> tuple().
make_shift_table(List) ->
    M = length(List),
    Op = fun ({V, K}, Ks) ->
                 case V + K =:= M of
                     true  -> [K | Ks];
                     false -> [hd(Ks) | Ks]
                 end
         end,
    Vks1 = lists:zip(tl(pfad_ch15:linear_allcp(List)) ++ [0],
                     lists:seq(1, M)),
    Vks2 = lists:zip(lists:seq(M, 1, -1),
                     lists:foldr(Op, [], Vks1)),
    Vks = lists:reverse(lists:sort(Vks1 ++ Vks2)),
    erlang:make_tuple(M + 1, M, [{V + 1, K} || {V, K} <- Vks]).

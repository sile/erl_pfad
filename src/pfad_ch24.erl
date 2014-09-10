%% @doc Chapter24: Rational arithmetic coding
-module(pfad_ch24).

-export([
         static_model/1,
         encode/2,
         decode/3,
         stream_encode/2
        ]).

-export([
         narrow/2,
         widen/2,
         ratio/2,
         ratio_mul/2, ratio_add/2, ratio_sub/2, ratio_div/2, ratio_floor/1,
         to_bits/1,
         to_frac/1
        ]).

-export_type([
              fraction/0
             ]).

-type ratio() :: {ratio, integer(), integer()}.
-type fraction() :: ratio().
-type interval() :: {fraction(), fraction()}.

-type symbol() :: term().
-type interval_fun() :: fun ((model(), symbol()) -> interval()).
-type symbol_fun() :: fun ((model(), fraction()) -> symbol()).
-type adapt_fun() :: fun ((model(), symbol()) -> model()).
-type model() :: #{data     => term(),
                   interval => interval_fun(),
                   symbol   => symbol_fun(),
                   adapt    => adapt_fun()}.

-spec narrow(interval(), interval()) -> interval().
narrow({L1, R1}, {L2, R2}) ->
    Delta = ratio_sub(R1, L1),
    {ratio_add(L1, ratio_mul(Delta, L2)), ratio_add(L1, ratio_mul(Delta, R2))}.

-spec widen(fraction(), interval()) -> fraction().
widen(F, {L, R}) ->
    ratio_div(ratio_sub(F, L), ratio_sub(R, L)).

-spec unit_interval() -> interval().
unit_interval() ->
    {ratio(0, 1), ratio(1, 1)}.

-spec intervals(model(), [symbol()]) -> [interval()].
intervals(_, [])       -> [];
intervals(M, [X | Xs]) ->
    #{interval := Interval, adapt := Adapt} = M,
    [Interval(M, X) | intervals(Adapt(M, X), Xs)].

%% ```
%% pfad_ch24:static_model([{e, {3,8}}, {g, {1,2}}, {n, {5,8}}, {r, {7,8}}, {v, {1,1}}]).
%% '''
-spec static_model([{symbol(), End}]) -> model() when
      End :: {Num::integer(), Denom::integer()}.
static_model(Xs) ->
    {_, Data} =
        lists:foldl(
          fun ({Symbol, End}, {Start, Acc}) ->
                  {End, [{Symbol, {ratio(Start), ratio(End)}} | Acc]}
          end,
          {{0,1}, []},
          Xs),
    #{
       data     => Data,
       interval => fun (_, S) -> proplists:get_value(S, Data) end,
       symbol   => fun (_, F) -> element(1, hd(lists:dropwhile(fun ({_, I}) -> not contain(I, F) end, Data))) end,
       adapt    => fun (M, _) -> M end
     }.

-spec encode(model(), [symbol()]) -> fraction().
encode(M, Symbols) ->
    pick(lists:foldl(fun (SymbolInterval, Acc) -> narrow(Acc, SymbolInterval) end,
                     unit_interval(),
                     intervals(M, Symbols))).

-spec decode(model(), fraction(), non_neg_integer()) -> [symbol()].
decode(M0, F, N0) ->
    Step =
        fun ({_, _, 0}) -> error;
            ({M, I, N}) ->
                #{symbol := Symbol, adapt := Adapt, interval := Interval} = M,
                X = Symbol(M, widen(F, I)),
                {ok, X, {Adapt(M, X), narrow(I, Interval(M, X)), N - 1}}
        end,
    pfad_util:unfoldr(Step, {M0, unit_interval(), N0}).

%% @equiv to_frac(to_bits({L,R}))
-spec pick(interval()) -> fraction().
pick({L, R}) ->
    One  = ratio(1, 1),
    Two  = ratio(2, 1),
    Half = ratio(1, 2),
    case {ratio_compare(R, Half), ratio_compare(Half, L)} of
        {1, 1} -> Half;
        {1, _} -> ratio_div(ratio_add(One, pick({ratio_sub(ratio_mul(Two, L), One), ratio_sub(ratio_mul(Two, R), One)})), Two);
        {_, _} -> ratio_div(pick({ratio_mul(Two, L), ratio_mul(Two, R)}), Two)
    end.

-spec contain(interval(), fraction()) -> boolean().
contain({L, R}, F) ->
    (ratio_compare(L, F) =< 0 andalso
     ratio_compare(F, R) < 0).

-spec ratio(integer(), integer()) -> ratio().
ratio(N, D) ->
    Sign = pfad_util:signum(N) * pfad_util:signum(D),
    Gcd = gcd(abs(N), abs(D)),
    {ratio, abs(N) div Gcd * Sign, abs(D) div Gcd}.

-spec ratio({integer(), integer()}) -> ratio().
ratio({N, D}) -> ratio(N, D).

-spec gcd(integer(), integer()) -> integer().
gcd(A, 0)            -> A;
gcd(A, B)            -> gcd(B, A rem B).

-spec ratio_add(ratio(), ratio()) -> ratio().
ratio_add({ratio, N1, D1}, {ratio, N2, D2}) ->
    ratio((N1 * D2) + (N2 * D1), D1 * D2).

-spec ratio_sub(ratio(), ratio()) -> ratio().
ratio_sub({ratio, N1, D1}, {ratio, N2, D2}) ->
    ratio((N1 * D2) - (N2 * D1), D1 * D2).

-spec ratio_mul(ratio(), ratio()) -> ratio().
ratio_mul({ratio, N1, D1}, {ratio, N2, D2}) ->
    ratio(N1 * N2, D1 * D2).

-spec ratio_div(ratio(), ratio()) -> ratio().
ratio_div(R, {ratio, N, D}) ->
    ratio_mul(R, {ratio, D, N}).

-spec ratio_floor(ratio()) -> integer().
ratio_floor({ratio, N, D}) ->
    N div D.

-spec ratio_compare(ratio(), ratio()) -> -1 | 0 | 1.
ratio_compare(A, B) ->
    {ratio, N, _} = ratio_sub(A, B),
    pfad_util:signum(N).

-type bit() :: 1 | 0.

-spec bit(interval()) -> {ok, bit(), interval()} | error.
bit({L, R}) ->
    One  = ratio(1, 1),
    Two  = ratio(2, 1),
    Half = ratio(1, 2),
    case {ratio_compare(R, Half), ratio_compare(Half, L)} of
        {1, 1} -> error;
        {1, _} -> {ok, 1, {ratio_sub(ratio_mul(Two, L), One), ratio_sub(ratio_mul(Two, R), One)}};
        {_, _} -> {ok, 0, {ratio_mul(Two, L), ratio_mul(Two, R)}}
    end.

-spec to_bits(interval()) -> [bit()].
to_bits(I) ->
    pfad_util:unfoldr(fun bit/1, I).

-spec to_frac([bit()]) -> fraction().
to_frac(Bits) ->
    lists:foldr(fun (B, F) -> ratio_div(ratio_add(ratio(B, 1), F), ratio(2, 1)) end,
                ratio(1, 2),
                Bits).

-spec stream_encode(model(), [symbol()]) -> [bit()].
stream_encode(M, Symbols) ->
    pfad_util:stream(fun bit/1, fun narrow/2, unit_interval(), intervals(M, Symbols)).

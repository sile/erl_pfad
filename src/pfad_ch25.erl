%% @doc Chapter25: Integer arithmetic coding
-module(pfad_ch25).

-export([
         static_model/1,
         test/0, test/1, test/2,

         encode_1/2,
         encode_2/2,
         encode_3/2,
         decode/3,
         p218_decode/3
        ]).

-define(E, 5).
-define(MAX_E, 32). % e=5
-define(MAX_D, 8).  % d=3

-define(MAX_E_MINUS_ONE, 31).

-type fixnum() :: 0..?MAX_E.
-type fixnum_minus_one() :: 0..?MAX_E_MINUS_ONE.
-type interval() :: {fixnum_minus_one(), fixnum()}.

-type bit()  :: 0 | 1.
-type bits() :: [bit()].

-type symbol() :: term().
-type interval_fun() :: fun ((model(), symbol()) -> interval()).
-type symbol_fun() :: fun ((model(), fixnum()) -> symbol()).
-type adapt_fun() :: fun ((model(), symbol()) -> model()).
-type model() :: #{data     => term(),
                   interval => interval_fun(),
                   symbol   => symbol_fun(),
                   adapt    => adapt_fun()}.

-type expanded_interval() :: {non_neg_integer(), interval()}.

-spec unit_interval() -> interval().
unit_interval() ->
    {0, ?MAX_E}.

-spec unit_expanded_interval() -> expanded_interval().
unit_expanded_interval() ->
    {0, unit_interval()}.

test() -> test([b, b, a]).

test(Symbols) -> test(static_model([{a, {3,5}}, {b, {5,6}}]), Symbols).

test(Model, Symbols) ->
    encode_3(Model, Symbols).

%% ```
%% pfad_ch25:static_model([{a, {3,5}}, {b, {5,6}}]).
%% '''
-spec static_model([{symbol(), {Start, End}}]) -> model() when
      Start :: 0..?MAX_D,
      End   :: 0..?MAX_D.
static_model(Data) ->
    #{
       data     => Data,
       interval => fun (_, S) -> proplists:get_value(S, Data) end,
       symbol   => fun (_, F) -> element(1, hd(lists:dropwhile(fun ({_, I}) -> not contain(I, F) end, Data))) end,
       adapt    => fun (M, _) -> M end
     }.

-spec contain(interval(), fixnum()) -> boolean().
contain({L, R}, F) ->
    L =< F andalso F < R.

-spec narrow(interval(), interval()) -> interval().
narrow({L, R}, {P, Q}) ->
    Delta = R - L,
    {L + Delta * P div ?MAX_D, L + Delta * Q div ?MAX_D}.

-spec ibit(interval()) -> {ok, bit(), interval()} | error.
ibit({L, R}) ->
    if
        R =< ?MAX_E bsr 1 -> {ok, 0, {2 * L, 2 * R}};
        ?MAX_E bsr 1 =< L -> {ok, 1, {2 * L - ?MAX_E, 2 * R - ?MAX_E}};
        true              -> error
    end.

-spec to_bits(interval()) -> [bit()].
to_bits(I) ->
    pfad_util:unfoldr(fun ibit/1, I).

-spec encode_1(model(), [symbol()]) -> bits().
encode_1(M, Symbols) ->
    to_bits(lists:foldl(fun narrow/2, unit_interval(), intervals(M, Symbols))).

-spec intervals(model(), [symbol()]) -> [interval()].
intervals(_, [])       -> [];
intervals(M, [X | Xs]) ->
    #{interval := Interval, adapt := Adapt} = M,
    [Interval(M, X) | intervals(Adapt(M, X), Xs)].

-spec encode_2(model(), [symbol()]) -> bits().
encode_2(M, Symbols) ->
    pfad_util:stream(fun ibit/1, fun narrow/2, unit_interval(), intervals(M, Symbols)).

-spec widen(fixnum(), pfad_ch24:fraction()) -> pfad_ch24:fraction().
widen(N, X) ->
    %% (1 bsl N) * (X - (?MAX_E bsr 1)) + (?MAX_E bsr 1)
    E = pfad_ch24:ratio(?MAX_E bsr 1, 1),
    pfad_ch24:ratio_add(
      pfad_ch24:ratio_mul(pfad_ch24:ratio(1 bsl N, 1), pfad_ch24:ratio_sub(X, E)),
      E).

%% -spec expand(interval()) -> expanded_interval().
%% expand(I) ->
%%     extend({0, I}).

-spec e(1..4) -> fixnum().
e(I) -> ?MAX_E * I div 4.

-spec extend(expanded_interval()) -> expanded_interval().
extend({N, {L, R}}) ->
    case e(1) =< L andalso R =< e(3) of
        true  -> extend({N + 1, {2 * L - e(2), 2 * R - e(2)}});
        false -> {N, {L, R}}
    end.

%% -spec contract(expanded_interval()) -> interval().
%% contract({N, {L, R}}) ->
%%     {shorten(N, L), shorten(N, R)}.

%% -spec shorten(non_neg_integer(), fixnum()) -> fixnum().
%% shorten(N, X) ->
%%     (X - e(2)) div (1 bsl N) + e(2).

-spec enarrow(expanded_interval(), interval()) -> expanded_interval().
enarrow(Ei, J) ->
    {N, I} = extend(Ei),
    {N, narrow(I, J)}.

-spec ebits(expanded_interval()) -> {ok, bits(), expanded_interval()} | error.
ebits({N, {L, R}}) ->
    case {R =< e(2), e(2) =< L} of
        {true, _} -> {ok, bits(N, 0), {0, {2 * L, 2 * R}}};
        {_, true} -> {ok, bits(N, 1), {0, {2 * L - e(4), 2 * R - e(4)}}};
        _         -> error
    end.

-spec bits(non_neg_integer(), bit()) -> bits().
bits(N, B) ->
    [B | lists:duplicate(N, 1 - B)].

%% p213
-spec encode_3(model(), [symbol()]) -> [bit()].
encode_3(M, Symbols) ->
    lists:append(pfad_util:stream(fun ebits/1, fun enarrow/2, unit_expanded_interval(), intervals(M, Symbols))).

-spec decode(model(), bits(), non_neg_integer()) -> [symbol()].
decode(M0, Bs0, C0) ->
    Step =
        fun Step({0, _, _, _})            -> error;
            Step({C, M, {N, {L, R}}, Bs}) ->
                case {R =< e(2), e(2) =< L} of
                    {true, _} -> Step({C, M, {0, {2 * L, 2 * R}},               lists:nthtail(length(bits(N, 0)), Bs)});
                    {_, true} -> Step({C, M, {0, {2 * L - e(4), 2 * R - e(4)}}, lists:nthtail(length(bits(N, 1)), Bs)});
                    _         ->
                        #{adapt := Adapt, interval := Interval} = M,
                        X = h({M, {N, {L, R}}}, Bs),
                        {ok, X, {C - 1, Adapt(M, X), enarrow({N, {L, R}}, Interval(M, X)), Bs}}
                end
        end,
    pfad_util:unfoldr(Step, {C0, M0, unit_expanded_interval(), Bs0}).

-spec interval_widen(non_neg_integer(), interval()) -> non_neg_integer().
interval_widen(K, {L, R}) ->
    ((K - L + 1) * ?MAX_D - 1) div (R - L).

-spec h({model(), expanded_interval()}, bits()) -> symbol().
h({M, Ei}, Bs) ->
    #{symbol := Symbol} = M,
    {N, I} = extend(Ei),
    F = interval_widen(
          pfad_ch24:ratio_floor(widen(N, pfad_ch24:ratio_mul(pfad_ch24:ratio(?MAX_E, 1), pfad_ch24:to_frac(Bs)))),
          I),
    Symbol(M, F).

-spec p218_decode(model(), bits(), non_neg_integer()) -> [symbol()].
p218_decode(M0, Bs0, C0) ->
    Bs1 = Bs0 ++ [1 | lists:duplicate(?E, 0)],
    ToInt = fun (Bs) ->
                    <<N:?E>> = << <<B:1>> || B <- Bs>>,
                    N
            end,
    Step =
        fun Step({0, _, _, _, _})             -> error;
            Step({C, M, {L, R}, N, [B | Bs]}) ->
                case {R =< e(2), e(2) =< L, e(1) =< L andalso R =< e(3)} of
                    {true, _, _} -> Step({C, M, {2 * L       , 2 * R       }, 2 * N        + B, Bs});
                    {_, true, _} -> Step({C, M, {2 * L - e(4), 2 * R - e(4)}, 2 * N - e(4) + B, Bs});
                    {_, _, true} -> Step({C, M, {2 * L - e(2), 2 * R - e(2)}, 2 * N - e(2) + B, Bs});
                    _            ->
                        #{symbol := Symbol, interval := Interval, adapt := Adapt} = M,
                        X = Symbol(M, interval_widen(N, {L, R})),
                        {ok, X, {C - 1, Adapt(M, X), narrow({L, R}, Interval(M, X)), N, [B | Bs]}}
                end
        end,
    pfad_util:unfoldr(
      Step, {C0, M0, unit_interval(), ToInt(pfad_util:take(?E, Bs1)), pfad_util:drop(?E, Bs1)}).

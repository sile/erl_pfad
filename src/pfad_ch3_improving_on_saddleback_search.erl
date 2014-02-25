%% @doc Chapter 3: Improving on saddleback search
-module(pfad_ch3_improving_on_saddleback_search).

-export([
         invert_p12_1/2,
         invert_p12_2/2,
         invert_p13_1/2,
         invert_p13_2/2
        ]).

-export_type([
              binary_fun/0,
              argument_pair/0
             ]).

-type binary_fun() :: fun ((non_neg_integer(), non_neg_integer()) -> non_neg_integer()).
-type argument_pair() :: {non_neg_integer(), non_neg_integer()}.

%% @doc (N) ^ 2 = O(N^2): N = Z + 1
-spec invert_p12_1(binary_fun(), non_neg_integer()) -> [argument_pair()].
invert_p12_1(Fun, Z) ->
    [{X, Y} || X <- lists:seq(0, Z), Y <- lists:seq(0, Z), Fun(X, Y) =:= Z].

%% @doc (N * (N+1)) / 2 = O(N^2): N = Z + 1
-spec invert_p12_2(binary_fun(), non_neg_integer()) -> [argument_pair()].
invert_p12_2(Fun, Z) ->
    [{X, Y} || X <- lists:seq(0, Z), Y <- lists:seq(0, Z - X), Fun(X, Y) =:= Z].

%% @doc (N) ^ 2 = O(N^2): N = Z + 1
-spec invert_p13_1(binary_fun(), non_neg_integer()) -> [argument_pair()].
invert_p13_1(Fun, Z) ->
    find_p13_1({0, Z}, Fun, Z).

-spec find_p13_1(argument_pair(), binary_fun(), non_neg_integer()) -> [argument_pair()].
find_p13_1({FirstX, LastY}, Fun, Z) ->
    [{X, Y} || X <- lists:seq(FirstX, Z), Y <- lists:seq(0, LastY), Fun(X, Y) =:= Z].

%% @doc O(N): N = Z + 1
-spec invert_p13_2(binary_fun(), non_neg_integer()) -> [argument_pair()].
invert_p13_2(Fun, Z) ->
    find_p13_2({0, Z}, Fun, Z).

-spec find_p13_2(argument_pair(), binary_fun(), non_neg_integer()) -> [argument_pair()].
find_p13_2({X, Y}, _, Z) when X > Z; Y < 0 ->
    [];
find_p13_2({X, Y}, Fun, Z) ->
    Result = Fun(X, Y),
    if
        Result  <  Z -> find_p13_2({X + 1, Y}, Fun, Z);
        Result =:= Z -> [{X, Y} | find_p13_2({X + 1, Y - 1}, Fun, Z)];
        Result  >  Z -> find_p13_2({X, Y - 1}, Fun, Z)
    end.

%% @doc Chapter10: Removing duplicates
-module(pfad_ch10).

-export([
         exponential_nub/1,
         quadratic_nub/1,
         n_log_n_nub/1
        ]).

-spec exponential_nub([term()]) -> [term()].
exponential_nub([])       -> [];
exponential_nub([X | Xs]) ->
    case lists:member(X, Xs) of
        false -> [X | exponential_nub(Xs)];
        true  -> min([X | exponential_nub('\\'(Xs, [X]))],
                     exponential_nub(Xs))
    end.

-spec '\\'([term()], [term()]) -> [term()].
'\\'(Xs, Ys) ->
    lists:filter(fun (X) -> not lists:member(X, Ys) end,
                 Xs).

-spec quadratic_nub([term()]) -> [term()].
quadratic_nub(List) ->
    hub([], List).

-spec hub([term()], [term()]) -> [term()].
hub(_, [])        -> [];
hub(Ws, [X | Xs]) ->
    {Us, Vs} = lists:partition(fun (W) -> W < X end, Ws),
    case {lists:member(X, Xs), lists:member(X, Ws)} of
        {false, false} -> Us ++ [X] ++ hub([], '\\'(Xs, Us));
        {false, true}  -> Us ++ [X] ++ hub(tl(Vs), '\\'(Xs, Us));
        {true, false}  -> hub(Us ++ [X], Xs);
        {true, true}   -> hub(Ws, Xs)
    end.

-spec n_log_n_nub([term()]) -> [term()].
n_log_n_nub(List) ->
    n_log_n_hub(sets:new(), sets:new(), preprocess(List)).

-spec preprocess([term()]) -> [{term(), sets:set()}].
preprocess(List) ->
    lists:zip(
      List,
      lists:foldr(fun (X, Acc) ->
                          [sets:add_element(X, hd(Acc)) | Acc]
                  end,
                  [sets:new()],
                  tl(List))).

-spec n_log_n_hub(sets:set(), sets:set(), [{term(), sets:set()}]) -> [term()].
n_log_n_hub(_, _, [])                -> [];
n_log_n_hub(Ps, Ws, [{X, Xs} | Xss]) ->
    case sets:is_element(X, Ps) of
        true  -> n_log_n_hub(Ps, Ws, Xss);
        false -> 
            {Us, Vs} = sets:fold(fun (W, {AccUs, AccYs}) ->
                                         case W < X of
                                             true  -> {sets:add_element(W, AccUs), AccYs};
                                             false -> {AccUs, sets:add_element(W, AccYs)}
                                         end
                                 end,
                                 {sets:new(), sets:new()},
                                 Ws),
            Eus = sets:to_list(Us),
            Qs = sets:union(Us, Ps),
            case {sets:is_element(X, Xs), sets:is_element(X, Ws)} of
                {false, false} -> Eus ++ [X] ++ n_log_n_hub(Qs, sets:new(), Xss);
                {false, true}  -> Eus ++ [X] ++ n_log_n_hub(Qs, Vs, Xss);
                {true, false}  -> n_log_n_hub(Ps, sets:add_element(X, Us), Xss);
                {true, true}   -> n_log_n_hub(Ps, Ws, Xss)
            end
    end.

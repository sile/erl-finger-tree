-module(ft_base).

-export([
         new/4,
         is_empty/1,
         is_finger_tree/2,
         reduce_l/3, reduce_r/3,
         push_l/2, push_r/2,
         pop_l/1, pop_r/1,
         head_l/1, head_r/1,
         tail_l/1, tail_r/1,
         append_list/2,
         to_list/1,
         concat/2,
         split/2, take_until/2, drop_until/2,
         measure/1
        ]).

-export_type([
              ft_base/0, ft_base/1,
              entry/0,
              
              reduce_fn/0, reduce_result_value/0,
              
              monoid_value/0,
              monoid_empty/0,
              monoid_append_fn/0,

              measure_fn/0,
              measure_value/0,
              measure_predicate_fn/0
             ]).

%%---------------------------------------------------------------------------------
%% Records
%%---------------------------------------------------------------------------------
-record(monoid,
        {
          empty  :: monoid_empty(),
          append :: monoid_append_fn()
        }).

-record(finger_tree,
        {
          name    :: atom(),
          monoid  :: monoid(),
          measure :: measure_fn(),
          root    :: root_tree()
        }).

%%---------------------------------------------------------------------------------
%% Types
%%---------------------------------------------------------------------------------
-type ft_base()  :: #finger_tree{}.
-type ft_base(A) :: #finger_tree{name :: A}.
-type entry()   :: term().

-type root_tree() :: tree(entry()).
-type tree(A)     :: empty |
                     {single, A} |
                     {deep, measure_value(), digit(A), tree(node(A)), digit(A)}.

-type node(A)  :: {node, measure_value(), A, A} |
                  {node, measure_value(), A, A, A}.
-type leaf()   :: entry() | node(leaf()).
-type digit(A) :: [A].

-type monoid()           :: #monoid{}.
-type monoid_value()     :: term().
-type monoid_empty()     :: monoid_value().
-type monoid_append_fn() :: fun((monoid_value(), monoid_value()) -> monoid_value()).

-type measure_value() :: monoid_value().
-type measure_fn()    :: fun((entry()) -> monoid_value()).
-type msr_pred_fn()   :: fun((measure_value()) -> boolean()).
-type msr_acc_value() :: term().
-type measure_predicate_fn() :: msr_pred_fn().

-type reduce_result_value() :: term().
-type reduce_fn()           :: fun((entry(), reduce_result_value()) -> reduce_result_value()).

%%---------------------------------------------------------------------------------
%% External Functions
%%---------------------------------------------------------------------------------
-spec new(atom(), monoid_empty(), monoid_append_fn(), measure_fn()) -> ft_base().
new(Name, MonoidEmpty, MonoidAppendFn, MeasureFn) ->
    #finger_tree{name    = Name,
                 monoid  = #monoid{empty = MonoidEmpty, append = MonoidAppendFn},
                 measure = MeasureFn,
                 root    = empty()}.

-spec is_empty(ft_base()) -> boolean().
is_empty(FT) -> FT#finger_tree.root =:= empty.

-spec is_finger_tree(ft_base(), atom()) -> boolean().
is_finger_tree(#finger_tree{name = Name}, Name) -> true;
is_finger_tree(_, _)                            -> false.

-spec reduce_l(reduce_fn(), reduce_result_value(), ft_base()) -> reduce_result_value().
reduce_l(Fn, Acc, FT) -> reduce_tree_l(Fn, Acc, FT#finger_tree.root).

-spec reduce_r(reduce_fn(), reduce_result_value(), ft_base()) -> reduce_result_value().
reduce_r(Fn, Acc, FT) -> reduce_tree_r(Fn, Acc, FT#finger_tree.root).

-spec push_l(entry(), ft_base()) -> ft_base().
push_l(Entry, FT) -> FT#finger_tree{root = push_l(FT, Entry, FT#finger_tree.root)}.

-spec push_r(entry(), ft_base()) -> ft_base().
push_r(Entry, FT) -> FT#finger_tree{root = push_r(FT, Entry, FT#finger_tree.root)}.

-spec pop_l(ft_base()) -> {entry(), ft_base()}.
pop_l(FT) ->
    {Head, Tail} = pop_l(FT, FT#finger_tree.root),
    {Head, FT#finger_tree{root = Tail}}.

-spec pop_r(ft_base()) -> {entry(), ft_base()}.
pop_r(FT) ->
    {Head, Tail} = pop_r(FT, FT#finger_tree.root),
    {Head, FT#finger_tree{root = Tail}}.

-spec head_l(ft_base()) -> entry().
head_l(FT) -> element(1, pop_l(FT)).

-spec head_r(ft_base()) -> entry().
head_r(FT) -> element(1, pop_r(FT)).

-spec tail_l(ft_base()) -> ft_base().
tail_l(FT) -> element(2, pop_l(FT)).

-spec tail_r(ft_base()) -> ft_base().
tail_r(FT) -> element(2, pop_r(FT)).

-spec append_list(ft_base(), [entry()]) -> ft_base().
append_list(FT, Entries) -> FT#finger_tree{root = append_r(FT, Entries, FT#finger_tree.root)}.

-spec to_list(ft_base()) -> [entry()].
to_list(FT) -> reduce_r(fun (Entry, Acc) -> [Entry | Acc] end, [], FT).

-spec concat(ft_base(), ft_base()) -> ft_base().
concat(FT1, FT2) -> FT1#finger_tree{root = concat(FT1, FT1#finger_tree.root, FT2#finger_tree.root)}.

-spec split(measure_predicate_fn(), ft_base()) -> {ft_base(), ft_base()}.
split(PredFn, FT) ->
    {L, R} = split(FT, PredFn, FT#finger_tree.root),
    {FT#finger_tree{root = L}, FT#finger_tree{root = R}}.

-spec take_until(measure_predicate_fn(), ft_base()) -> ft_base().
take_until(PredFn, FT) -> element(1, split(PredFn, FT)).

-spec drop_until(measure_predicate_fn(), ft_base()) -> ft_base().
drop_until(PredFn, FT) -> element(2, split(PredFn, FT)).
     
-spec measure(ft_base()) -> measure_value().
measure(FT) -> measure_tree(FT, FT#finger_tree.root).

%%---------------------------------------------------------------------------------
%% Internal Functions: Tree Element Constructor
%%---------------------------------------------------------------------------------
-spec node(ft_base(), X, X) -> node(X) when X :: leaf().
node(FT, A, B) ->
    Msr = monoid_append(FT, measure_leaf(FT, A), measure_leaf(FT, B)),
    {node, Msr, A, B}.

-spec node(ft_base(), X, X, X) -> node(X) when X :: leaf().
node(FT, A, B, C) ->
    Msr = monoid_concat(FT, measure_leaf_list(FT, [A,B,C])),
    {node, Msr, A, B, C}.

-spec empty() -> tree(leaf()).
empty() -> empty.

-spec single(X) -> tree(X) when X :: leaf().
single(A) -> {single, A}.

-spec deep(ft_base(), digit(X), tree(node(X)), digit(X)) -> tree(X) when X :: leaf().
deep(FT, Pr, M, Sf) ->
    Msr = monoid_concat(FT, measure_leaf_list(FT, Pr) ++ [measure_tree(FT, M)] ++ measure_leaf_list(FT, Sf)),
    deep(FT, Msr, Pr, M, Sf).

-spec deep(ft_base(), measure_value(), digit(X), tree(X), digit(X)) -> tree(X) when X :: leaf().
deep(_FT, Msr, Pr, M, Sf) ->
    {deep, Msr, Pr, M, Sf}.

-spec deep_l(ft_base(), []|digit(X), tree(node(X)), digit(X)) -> tree(X) when X :: leaf().
deep_l(FT, [], empty, Sf) ->
    digit_to_tree(FT, Sf);
deep_l(FT, [], M, Sf) ->
    {Head, Tail} = pop_l(FT, M),
    deep(FT, node_to_digit(Head), Tail, Sf);
deep_l(FT, Pr, M, Sf) ->
    deep(FT, Pr, M, Sf).

-spec deep_r(ft_base(), digit(X), tree(node(X)), []|digit(X)) -> tree(X) when X :: leaf().
deep_r(Tree, Pr, empty, []) ->
    digit_to_tree(Tree, Pr);
deep_r(Tree, Pr, M, []) ->
    {Head, Tail} = pop_r(Tree, M),
    deep(Tree, Pr, Tail, node_to_digit(Head));
deep_r(Tree, Pr, M, Sf) ->
    deep(Tree, Pr, M, Sf).

-spec digit_to_tree(ft_base(), []|digit(X)) -> tree(X) when X :: leaf().
digit_to_tree(FT, Digit) ->
    case Digit of
        []        -> empty();
        [A]       -> single(A);
        [A,B]     -> deep(FT, [A], empty, [B]);
        [A,B,C]   -> deep(FT, [A,B], empty, [C]);
        [A,B,C,D] -> deep(FT, [A,B], empty, [C,D])
    end.

-spec node_to_digit(node(X)) -> digit(X) when X :: leaf().
node_to_digit({node, _, A, B})    -> [A, B];
node_to_digit({node, _, A, B, C}) -> [A, B, C].

%%---------------------------------------------------------------------------------
%% Internal Functions: Reduction
%%---------------------------------------------------------------------------------
-spec reduce_tree_l(reduce_fn(), reduce_result_value(), tree(X)) -> reduce_result_value() when X :: leaf().
reduce_tree_l(Fn, Acc, Tree) ->
    case Tree of
        empty                -> Acc;
        {single, A}          -> reduce_leaf_l(Fn, Acc, A);
        {deep, _, Pr, M, Sf} -> Acc1 = reduce_digit_l(Fn, Acc, Pr),
                                Acc2 = reduce_tree_l(Fn, Acc1, M),
                               _Acc3 = reduce_digit_l(Fn, Acc2, Sf)
    end.

-spec reduce_leaf_l(reduce_fn(), reduce_result_value(), leaf()) -> reduce_result_value().
reduce_leaf_l(Fn, Acc, Leaf) ->
    case Leaf of
        {node, _, A, B}    -> reduce_leaf_l(Fn, reduce_leaf_l(Fn, Acc, A), B);
        {node, _, A, B, C} -> reduce_leaf_l(Fn, reduce_leaf_l(Fn, reduce_leaf_l(Fn, Acc, A), B), C);
        Entry              -> Fn(Entry, Acc)
    end.

-spec reduce_digit_l(reduce_fn(), reduce_result_value(), digit(X)) -> reduce_result_value() when X :: leaf().
reduce_digit_l(Fn, Acc, Digit) ->
    ReduceFn = fun (Leaf, AccTmp) -> reduce_leaf_l(Fn, AccTmp, Leaf) end,
    lists:foldl(ReduceFn, Acc, Digit).

-spec reduce_tree_r(reduce_fn(), reduce_result_value(), tree(X)) -> reduce_result_value() when X :: leaf().
reduce_tree_r(Fn, Acc, Node) ->
    case Node of
        empty                -> Acc;
        {single, A}          -> reduce_leaf_r(Fn, Acc, A);
        {deep, _, Pr, M, Sf} -> Acc1 = reduce_digit_r(Fn, Acc, Sf),
                                Acc2 = reduce_tree_r(Fn, Acc1, M),
                               _Acc3 = reduce_digit_r(Fn, Acc2, Pr)
    end.

-spec reduce_leaf_r(reduce_fn(), reduce_result_value(), leaf()) -> reduce_result_value().
reduce_leaf_r(Fn, Acc, Leaf) ->
    case Leaf of
        {node, _, A, B}    -> reduce_leaf_r(Fn, reduce_leaf_r(Fn, Acc, B), A);
        {node, _, A, B, C} -> reduce_leaf_r(Fn, reduce_leaf_r(Fn, reduce_leaf_r(Fn, Acc, C), B), A);
        Entry              -> Fn(Entry, Acc)
    end.

-spec reduce_digit_r(reduce_fn(), reduce_result_value(), digit(X)) -> reduce_result_value() when X :: leaf().
reduce_digit_r(Fn, Acc, Digit) ->
    ReduceFn = fun (Leaf, AccTmp) -> reduce_leaf_r(Fn, AccTmp, Leaf) end,
    lists:foldr(ReduceFn, Acc, Digit).

%%---------------------------------------------------------------------------------
%% Internal Functions: Edge Operation
%%---------------------------------------------------------------------------------
-spec push_l(ft_base(), X, tree(X)) -> tree(X) when X :: leaf().
push_l(FT, A, Tree) ->
    case Tree of
        empty                  -> single(A);
        {single, B}            -> deep(FT, [A], empty(), [B]);
        {deep, Msr, Pr, M, Sf} ->
            Msr2 = monoid_append(FT, measure_leaf(FT, A), Msr),
            case Pr of
                [B,C,D,E] -> deep(FT, Msr2, [A,B], push_l(FT, node(FT, C, D, E), M), Sf);
                _         -> deep(FT, Msr2, [A|Pr], M, Sf)
            end
    end.

-spec push_r(ft_base(), X, tree(X)) -> tree(X) when X :: leaf().
push_r(FT, A, Tree) ->
    case Tree of
        empty                  -> single(A);
        {single, B}            -> deep(FT, [B], empty(), [A]);
        {deep, Msr, Pr, M, Sf} ->
            Msr2 = monoid_append(FT, Msr, measure_leaf(FT, A)),
            case Sf of
                [E,D,C,B] -> deep(FT, Msr2, Pr, push_r(FT, node(FT, E, D, C), M), [B,A]);
                _         -> deep(FT, Msr2, Pr, M, Sf++[A])
            end
    end.

-spec pop_l(ft_base(), tree(X)) -> {X, tree(X)} when X :: leaf().
pop_l(FT, Tree) ->
    case Tree of
        {single, A}              -> {A, empty()};
        {deep, _, [A|Pr], M, Sf} -> {A, deep_l(FT, Pr, M, Sf)}
    end.

-spec pop_r(ft_base(), tree(X)) -> {X, tree(X)} when X :: leaf().
pop_r(FT, Tree) ->
    case Tree of
        {single, A}          -> {A, empty()};
        {deep, _, Pr, M, Sf} ->
            [Head | SfTail] = lists:reverse(Sf),
            {Head, deep_r(FT, Pr, M, lists:reverse(SfTail))}
    end.

-spec append_l(ft_base(), [X], tree(X)) -> tree(X) when X :: leaf().
append_l(FT, As, Tree) ->
    lists:foldr(fun (A, Acc) -> push_l(FT, A, Acc) end, Tree, As).

-spec append_r(ft_base(), [X], tree(X)) -> tree(X) when X :: leaf().
append_r(FT, As, Tree) ->
    lists:foldl(fun (A, Acc) -> push_r(FT, A, Acc) end, Tree, As).

%%---------------------------------------------------------------------------------
%% Internal Functions: Concatenation
%%---------------------------------------------------------------------------------
-spec concat(ft_base(), tree(X), tree(X)) -> tree(X) when X :: leaf().
concat(FT, Tree1, Tree2) ->
    app3(FT, Tree1, [], Tree2).

-spec app3(ft_base(), tree(X), [X], tree(X)) -> tree(X) when X :: leaf().
app3(FT, empty, Nodes, Tree)       -> append_l(FT, Nodes, Tree);
app3(FT, {single, A}, Nodes, Tree) -> append_l(FT, [A | Nodes], Tree);
app3(FT, Tree, Nodes, empty)       -> append_r(FT, Nodes, Tree);
app3(FT, Tree, Nodes, {single, A}) -> append_r(FT, Nodes++[A], Tree);
app3(FT, {deep, _, Pr1, M1, Sf1}, Nodes, {deep, _, Pr2, M2, Sf2}) ->
    M3 = app3(FT, M1, nodes(FT, Sf1 ++ Nodes ++ Pr2), M2),
    deep(FT, Pr1, M3, Sf2).

-spec nodes(ft_base(), [X]) -> [node(X)] when X :: leaf().
nodes(FT, [A,B])        -> [node(FT,A,B)];
nodes(FT, [A,B,C])      -> [node(FT,A,B,C)];
nodes(FT, [A,B,C,D])    -> [node(FT,A,B), node(FT,C,D)];
nodes(FT, [A,B,C | Xs]) -> [node(FT,A,B,C) | nodes(FT,Xs)].

%%---------------------------------------------------------------------------------
%% Internal Functions: Splitting
%%---------------------------------------------------------------------------------
-spec split(ft_base(), msr_pred_fn(), root_tree()) -> {root_tree(), root_tree()}.
split(_FT,_Pred, empty) -> {empty(), empty()};
split( FT, Pred, Tree) -> 
    case Pred(measure_tree(FT, Tree)) of
        true ->
            {L, X, R} = split_tree(FT, Pred, monoid_empty(FT), Tree),
            {L, push_l(FT, X, R)};
        false ->
            {Tree, empty()}
    end.

-spec split_tree(ft_base(), msr_pred_fn(), msr_acc_value(), tree(X)) -> {tree(X), X, tree(X)} when X :: leaf().
split_tree(_FT,_Pred,_Acc, {single, A}) -> {empty(), A, empty()};
split_tree( FT, Pred, Acc, Tree)        ->    
    {deep, _, Pr, M, _} = Tree,
    AccPr = monoid_concat(FT, [Acc | measure_leaf_list(FT, Pr)]),
    case Pred(AccPr) of
        true  -> split_deep_left(FT, Pred, Acc, Tree);
        false ->
            AccM = monoid_append(FT, AccPr, measure_tree(FT, M)),
            case Pred(AccM) of
                true  -> split_deep_middle(FT, Pred, AccPr, Tree);
                false -> split_deep_right(FT, Pred, AccM, Tree)
            end
    end.

-spec split_deep_left(ft_base(), msr_pred_fn(), msr_acc_value(), tree(X)) -> {tree(X), X, tree(X)} when X :: leaf().
split_deep_left(FT, Pred, Acc, {deep, _, Pr, M, Sf}) ->
    {L, X, R} = split_digit(FT, Pred, Acc, Pr),
    {digit_to_tree(FT, L), X, deep_l(FT, R, M, Sf)}.

-spec split_deep_middle(ft_base(), msr_pred_fn(), msr_acc_value(), tree(X)) -> {tree(X), X, tree(X)} when X :: leaf().
split_deep_middle(FT, Pred, Acc, {deep, _, Pr, M, Sf}) ->
    {Ml, SubX, Mr} = split_tree(FT, Pred, Acc, M),
    AccMl          = monoid_append(FT, Acc, measure_tree(FT, Ml)),
    {L, X, R}      = split_digit(FT, Pred, AccMl, node_to_digit(SubX)),
    {deep_r(FT, Pr, Ml, L), X, deep_l(FT, R, Mr, Sf)}.

-spec split_deep_right(ft_base(), msr_pred_fn(), msr_acc_value(), tree(X)) -> {tree(X), X, tree(X)} when X :: leaf().
split_deep_right(FT, Pred, Acc, {deep, _, Pr, M, Sf}) ->
    {L, X, R} = split_digit(FT, Pred, Acc, Sf),
    {deep_r(FT, Pr, M, L), X, digit_to_tree(FT, R)}.

-spec split_digit(ft_base(), msr_pred_fn(), msr_acc_value(), digit(X)) -> {[]|digit(X), X, []|digit(X)} when X :: leaf().
split_digit(_FT,_Pred,_Acc, [A])    -> {[], A, []};
split_digit( FT, Pred, Acc, [A|As]) ->
    Acc2 = monoid_append(FT, Acc, measure_leaf(FT, A)),
    case Pred(Acc2) of
        true  -> {[], A, As};
        false -> {L, X, R} = split_digit(FT, Pred, Acc2, As),
                 {[A|L], X, R}
    end.

%%---------------------------------------------------------------------------------
%% Internal Functions: Measure and Monoid
%%---------------------------------------------------------------------------------
-spec monoid_empty(ft_base()) -> monoid_empty().
monoid_empty(FT) -> (FT#finger_tree.monoid)#monoid.empty.

-spec monoid_append(ft_base(), monoid_value(), monoid_value()) -> monoid_value().
monoid_append(FT, A, B) -> ((FT#finger_tree.monoid)#monoid.append)(A, B).

-spec monoid_concat(ft_base(), [monoid_value()]) -> monoid_value().
monoid_concat(FT, As) ->
    lists:foldl(fun (A, Acc) -> monoid_append(FT, Acc, A) end,
                monoid_empty(FT),
                As).
                        
-spec measure_leaf(ft_base(), leaf()) -> measure_value().
measure_leaf(FT, A) ->
    case A of
        {node, M, _, _}    -> M;
        {node, M, _, _, _} -> M;
        _                  -> (FT#finger_tree.measure)(A)
    end.

-spec measure_tree(ft_base(), tree(X)) -> measure_value() when X :: leaf().
measure_tree(FT, Tree) ->
    case Tree of
        empty              -> monoid_empty(FT);
        {single, A}        -> measure_leaf(FT, A);
        {deep, M, _, _, _} -> M
    end.

-spec measure_leaf_list(ft_base(), [leaf()]) -> [measure_value()].
measure_leaf_list(FT, Leafs) -> [measure_leaf(FT, A) || A <- Leafs].

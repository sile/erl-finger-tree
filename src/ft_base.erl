-module(ft_base).

-export([
         monoid/2,
         new/2,
         push_l/2, push_r/2,
         pop_l/1, pop_r/1,
         head_l/1, head_r/1,
         tail_l/1, tail_r/1,
         from_list/2,
         to_list/1,
         reduce_l/3, reduce_r/3,
         concat/2,
         split/2, split_tree/2,
         measure/1
        ]).

-record(monoid,
        {
          empty  :: term(),
          append :: function()
        }).

-record(finger_tree,
        {
          monoid  :: monoid(),
          measure :: function(),
          root    :: ft_node()
        }).

-type monoid()  :: #monoid{}.
-type ft_node() :: term(). % TODO

monoid(Empty, AppendFn) ->
    #monoid{empty = Empty, append = AppendFn}.

new(Monoid, MeasureFn) ->
    #finger_tree{monoid  = Monoid,
                 measure = MeasureFn,
                 root    = empty()}.

empty() ->
    empty.

single(A) ->
    {single, A}.

from_list(Tree, List) ->
    Root = lists:foldl(fun (Elem, Node) -> push_r(Tree, Node, {value, Elem}) end,
                       Tree#finger_tree.root,
                       List),
    Tree#finger_tree{root = Root}.

to_list(Tree) ->
    reduce_r(fun (Elem, Acc) -> [Elem | Acc] end, [], Tree).
                               
push_l(Tree, Node, A) ->
    case Node of
        empty       -> single(A);
        {single, B} -> deep(Tree, [A], empty(), [B]);
        {deep, Msr, [B,C,D,E], M, Sf} -> deep(Tree, mpush_l(Tree, A, Msr), [A,B], push_l(Tree, M, node(Tree, C, D, E)), Sf);
        {deep, Msr, Pr, M, Sf}        -> deep(Tree, mpush_l(Tree, A, Msr), [A|Pr], M, Sf)
    end.

push_l(Tree, A) ->
    Root = push_l(Tree, Tree#finger_tree.root, {value, A}),
    Tree#finger_tree{root = Root}.

push_r(Tree, Node, A) ->
    case Node of
        empty -> single(A);
        {single, B} -> deep(Tree, [B], empty(), [A]);
        {deep, Msr, Pr, M, [E,D,C,B]} -> deep(Tree, mpush_r(Tree, Msr, A), Pr, push_r(Tree, M, node(Tree, E, D, C)), [B,A]);
        {deep, Msr, Pr, M, Sf}        -> deep(Tree, mpush_r(Tree, Msr, A), Pr, M, Sf++[A])
    end.

push_r(Tree, A) ->
    Root = push_r(Tree, Tree#finger_tree.root, {value, A}),
    Tree#finger_tree{root = Root}.

pop_l(Tree) ->
    {{value, Head}, Tail} = pop_l(Tree, Tree#finger_tree.root),
    {Head, Tree#finger_tree{root = Tail}}.

pop_l(Tree, Node) ->
    case Node of
        {single, A}              -> {A, empty()};
        {deep, _, [A|Pr], M, Sf} ->
            Head = A,
            Tail = deep_l(Tree, Pr, M, Sf),
            {Head, Tail}
    end.

pop_r(Tree) ->
    {{value, Head}, Tail} = pop_r(Tree, Tree#finger_tree.root),
    {Head, Tree#finger_tree{root = Tail}}.

pop_last(List) ->
    pop_last(List, []).

pop_last([X], Acc) ->
    {X, lists:reverse(Acc)};
pop_last([X|Xs], Acc) ->
    pop_last(Xs, [X|Acc]).

pop_r(Tree, Node) ->
    case Node of
        {single, A} -> {A, empty()};
        {deep, _, Pr, M, Sf_tmp} ->
            {Head, Sf} = pop_last(Sf_tmp),
            Tail = deep_r(Tree, Pr, M, Sf),
            {Head, Tail}
    end.

deep_r(Tree, Pr, empty, []) ->
    list_to_node(Tree, Pr);
deep_r(Tree, Pr, M, []) ->
    {Head, Tail} = pop_r(Tree, M),
    deep(Tree, Pr, Tail, node_to_list(Head));
deep_r(Tree, Pr, M, Sf) ->
    deep(Tree, Pr, M, Sf).

deep_l(Tree, [], empty, Sf) ->
    list_to_node(Tree, Sf);
deep_l(Tree, [], M, Sf) ->
    {Head, Tail} = pop_l(Tree, M),
    deep(Tree, node_to_list(Head), Tail, Sf);
deep_l(Tree, Pr, M, Sf) ->
    deep(Tree, Pr, M, Sf).

node_to_list({node, _, A, B}) -> [A, B];
node_to_list({node, _, A, B, C}) -> [A, B, C].

%% XXX: name
list_to_node(_Tree,[]) -> empty();
list_to_node(_Tree,[A]) -> single(A);
list_to_node(Tree, [A,B]) -> deep(Tree, [A], empty, [B]);
list_to_node(Tree, [A,B,C]) -> deep(Tree, [A, B], empty, [C]);
list_to_node(Tree, [A,B,C,D]) -> deep(Tree, [A,B], empty, [C,D]).

head_l(Tree) -> element(1, pop_l(Tree)).
head_r(Tree) -> element(1, pop_r(Tree)).

tail_l(Tree) -> element(2, pop_l(Tree)).
tail_r(Tree) -> element(2, pop_r(Tree)).

split_digit(_Tree,_Pred,_Acc, [A])    -> {split, [], A, []};
split_digit( Tree, Pred, Acc, [A|As]) ->
    Acc2 = mpush_r(Tree, Acc, A),
    case Pred(Acc2) of
        true  -> {split, [], A, As};
        false ->
            {split, L, X, R} = split_digit(Tree, Pred, Acc2, As),
            {split, [A|L], X, R}
    end.

split_tree(_Tree,_Pred,_Acc, {single, X}) -> {split, empty(), X, empty()};
split_tree( Tree, Pred, Acc, {deep, _, Pr, M, Sf}) ->
    Acc2 = mconcat(Tree, Acc, Pr),
    case Pred(Acc2) of
        true ->
            {split, L, X, R} = split_digit(Tree, Pred, Acc, Pr),
            {split, list_to_node(Tree, L), X, deep_l(Tree, R, M, Sf)};
        false ->
            Acc3 = mpush_r(Tree, Acc2, M),
            case Pred(Acc3) of
                true ->
                    {split, Ml, Xs, Mr} = split_tree(Tree, Pred, Acc2, M),
                    {split, L, X, R} = split_digit(Tree, Pred, mpush_r(Tree, Acc2, Ml), node_to_list(Xs)),
                    {split, deep_r(Tree, Pr, Ml, L), X, deep_l(Tree, R, Mr, Sf)};
                false ->
                    {split, L, X, R} = split_digit(Tree, Pred, Acc3, Sf),
                    {split, deep_r(Tree, Pr, M, L), X, list_to_node(Tree, R)}
            end
    end.

split(Tree, Pred) ->
    {L, R} = split(Tree, Pred, Tree#finger_tree.root),
    {Tree#finger_tree{root = L}, Tree#finger_tree{root = R}}.

split(_Tree,_Pred, empty) -> {empty(), empty()};
split(Tree, Pred, Node) -> 
    case Pred(measure(Tree, Node)) of
        true ->
            {split, L, X, R} = split_tree(Tree, Pred, mempty(Tree), Node),
            {L, push_l(Tree, R, X)};
        false ->
            {Node, empty()}
    end.

split_tree(Tree, Pred) ->
    {split, L, X, R} = split_tree(Tree, Pred, mempty(Tree), Tree#finger_tree.root),
    {Tree#finger_tree{root = L}, X, Tree#finger_tree{root = R}}.

concat(Tree1, Tree2) ->
    Tree1#finger_tree{root = app3(Tree1, Tree1#finger_tree.root, [], Tree2#finger_tree.root)}.

app3(Tree, empty, Ts, Node) ->
    lists:foldl(fun (X, Acc) -> push_l(Tree, Acc, X) end, Node, Ts);
app3(Tree, Node, Ts, empty) ->
    lists:foldl(fun (X, Acc) -> push_r(Tree, Acc, X) end, Node, Ts);
app3(Tree, {single, Y}, Ts, Node) ->
    push_l(Tree, lists:foldl(fun (X, Acc) -> push_l(Tree, Acc, X) end, Node, Ts), Y);
app3(Tree, Node, Ts, {single, Y}) ->
    push_r(Tree, lists:foldl(fun (X, Acc) -> push_r(Tree, Acc, X) end, Node, Ts), Y);
app3(Tree, {deep, _, Pr1, M1, Sf1}, Ts, {deep, _, Pr2, M2, Sf2}) ->
    deep(Tree, Pr1, app3(Tree, M1, nodes(Tree, Sf1 ++ Ts ++ Pr2), M2), Sf2).

nodes(Tree, [A,B]) -> [node(Tree, A, B)];
nodes(Tree, [A,B,C]) -> [node(Tree, A, B, C)];
nodes(Tree, [A,B,C,D]) -> [node(Tree,A,B), node(Tree,C,D)];
nodes(Tree, [A,B,C | Xs]) -> [node(Tree,A,B,C) | nodes(Tree, Xs)].

reduce_l(Fn, Acc, Tree) ->
    reduce_node_l(Fn, Acc, Tree#finger_tree.root).

reduce_node_l(Fn, Acc, Node) ->
    case Node of
        empty                -> Acc;
        {value, A}           -> Fn(A, Acc);
        {single, A}          -> reduce_node_l(Fn, Acc, A);
        {node, _, A, B}      -> reduce_node_l(Fn, reduce_node_l(Fn, Acc, A), B);
        {node, _, A, B, C}   -> reduce_node_l(Fn, reduce_node_l(Fn, reduce_node_l(Fn, Acc, A), B), C);
        {deep, _, Pr, M, Sf} ->
            lists:foldl(fun (SubNode, SubAcc) -> reduce_node_l(Fn, SubAcc, SubNode) end, 
                        Acc, 
                        Pr ++ [M] ++ Sf)
    end.

reduce_r(Fn, Acc, Tree) ->
    reduce_node_r(Fn, Acc, Tree#finger_tree.root).

reduce_node_r(Fn, Acc, Node) ->
    case Node of
        empty                -> Acc;
        {value, A}           -> Fn(A, Acc);
        {single, A}          -> reduce_node_r(Fn, Acc, A);
        {node, _, A, B}      -> reduce_node_r(Fn, reduce_node_r(Fn, Acc, B), A);
        {node, _, A, B, C}   -> reduce_node_r(Fn, reduce_node_r(Fn, reduce_node_r(Fn, Acc, C), B), A);
        {deep, _, Pr, M, Sf} ->
            lists:foldr(fun (SubNode, SubAcc) -> reduce_node_r(Fn, SubAcc, SubNode) end,
                        Acc,
                        Pr ++ [M] ++ Sf)
    end.

node(Tree, A, B) ->
    {node, mappend(Tree, A, B), A, B}.

node(Tree, A, B, C) ->
    {node, mconcat(Tree, [A,B,C]), A, B, C}.

deep(_Tree, Msr, Pr, M, Sf) ->
    {deep, Msr, Pr, M, Sf}.

deep(Tree, Pr, M, Sf) ->
    {deep, mconcat(Tree, Pr++[M]++Sf), Pr, M, Sf}.

mempty(Tree) ->
    (Tree#finger_tree.monoid)#monoid.empty.

measure(Tree) ->
    measure(Tree, Tree#finger_tree.root).

measure(Tree, X) ->
    case X of
        empty              -> mempty(Tree);
        {single, A}        -> measure(Tree, A);
        {node, M, _, _}    -> M;
        {node, M, _, _, _} -> M;
        {deep, M, _, _, _} -> M;
        {value, A}         -> (Tree#finger_tree.measure)(A)
    end.

mpush_r(Tree, A, B) ->
    Append = (Tree#finger_tree.monoid)#monoid.append,
    Append(A, measure(Tree, B)).

mpush_l(Tree, A, B) ->
    Append = (Tree#finger_tree.monoid)#monoid.append,
    Append(measure(Tree, A), B).
    
mappend(Tree, A, B) ->
    Append = (Tree#finger_tree.monoid)#monoid.append,
    Append(measure(Tree, A), measure(Tree, B)).

mconcat(Tree, Init, List) ->
    #monoid{append = Append} = Tree#finger_tree.monoid,
    lists:foldl(fun (X, Acc) ->
                        Append(Acc, measure(Tree, X))
                end,
                Init,
                List).

mconcat(Tree, List) ->
    mconcat(Tree, mempty(Tree), List).

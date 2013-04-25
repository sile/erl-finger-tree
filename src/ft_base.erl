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
         reduce_l/3, reduce_r/3
        ]).

-record(ft_monoid,
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

-type monoid()  :: #ft_monoid{}.
-type ft_node() :: term(). % TODO

monoid(Empty, AppendFn) ->
    #ft_monoid{empty = Empty, append = AppendFn}.

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

list_to_node(_Tree,[A]) -> single(A);
list_to_node(Tree, [A,B]) -> deep(Tree, [A], empty, [B]);
list_to_node(Tree, [A,B,C]) -> deep(Tree, [A, B], empty, [C]);
list_to_node(Tree, [A,B,C,D]) -> deep(Tree, [A,B], empty, [C,D]).

head_l(Tree) -> element(1, pop_l(Tree)).
head_r(Tree) -> element(1, pop_r(Tree)).

tail_l(Tree) -> element(2, pop_l(Tree)).
tail_r(Tree) -> element(2, pop_r(Tree)).


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

%node(Tree, A, B) ->
%    {node, mappend(Tree, A, B), A, B}.

node(Tree, A, B, C) ->
    {node, mconcat(Tree, [A,B,C]), A, B, C}.

deep(_Tree, Msr, Pr, M, Sf) ->
    {deep, Msr, Pr, M, Sf}.

deep(Tree, Pr, M, Sf) ->
    {deep, mconcat(Tree, Pr++[M]++Sf), Pr, M, Sf}.

measure(Tree, X) ->
    case X of
        empty              -> (Tree#finger_tree.monoid)#ft_monoid.empty;
        {single, A}        -> measure(Tree, A);
        {node, M, _, _}    -> M;
        {node, M, _, _, _} -> M;
        {deep, M, _, _, _} -> M;
        {value, A}         -> (Tree#finger_tree.measure)(A)
    end.

mpush_r(Tree, A, B) ->
    Append = (Tree#finger_tree.monoid)#ft_monoid.append,
    Append(A, measure(Tree, B)).

mpush_l(Tree, A, B) ->
    Append = (Tree#finger_tree.monoid)#ft_monoid.append,
    Append(measure(Tree, A), B).
    
%mappend(Tree, A, B) ->
%    Append = (Tree#finger_tree.monoid)#ft_monoid.append,
%    Append(measure(Tree, A), measure(Tree, B)).

mconcat(Tree, List) ->
    #ft_monoid{append = Append, empty = Empty} = Tree#finger_tree.monoid,
    lists:foldl(fun (X, Acc) ->
                        Append(Acc, measure(Tree, X))
                end,
                Empty,
                List).

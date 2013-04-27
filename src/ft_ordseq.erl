-module(ft_ordseq).

-export([
         new/0,
         partition/2,
         insert/2,
         delete/2,
         from_list/1,
         to_list/1,
         merge/2,
         pop/1
        ]).

new() ->
    Empty   = no_key,
    Append  = fun (K, no_key) -> K;
                  (_, K)      -> K
              end,
    Measure = fun (X) -> {key, X} end,
    ft_base:new(?MODULE, Empty, Append, Measure).

partition(Set, PivotElem) ->
    ft_base:split(Set, fun (K) ->  K >= {key, PivotElem} end).

insert(Set, Elem) ->
    {L, R} = partition(Set, Elem),
    ft_base:concat(L, ft_base:push_l(R, Elem)).

delete(Set, Elem) ->
    {L, R} = partition(Set, Elem),
    {_, R2} = ft_base:split(R, fun (K) -> K > {key, Elem} end),
    ft_base:concat(L, R2).

%% XXX: sort => push_lの方が効率的
from_list(List) ->
    lists:foldl(fun (X, Acc) -> insert(Acc, X) end,
                new(),
                List).

to_list(Set) ->
    ft_base:to_list(Set).

pop(Set) ->
    ft_base:pop_l(Set).

merge(Set1, Set2) ->
    case ft_base:is_empty(Set1) of
        true  -> Set2;
        false ->
            {Head1, Tail1} = pop(Set1),
            {L, R} = ft_base:split(Set2, fun (K) -> K > {key, Head1} end),
            ft_base:concat(L, ft_base:push_l(merge(Tail1, R), Head1))
    end.

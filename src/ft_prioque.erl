-module(ft_prioque).

-export([
         new/0,
         push/3,
         pop/1,
         from_list/1,
         max/1
        ]).

new() ->
    Empty   = minus_infinity,
    Append  = fun (minus_infinity, A) -> A;
                  (A, minus_infinity) -> A;
                  ({S1,_}=A, {S2,_}=B) ->
                      case S1 < S2 of
                          true  -> B;
                          false -> A
                      end
              end,
    Measure = fun (X) -> X end,
    ft_base:new(ft_base:monoid(Empty, Append), Measure).

push(Que, Score, Elem) ->
    ft_base:push_l(Que, {Score, Elem}).

pop(Que) ->
    Max = max(Que),
    {L, X, R} = ft_base:split_tree(Que, fun (X) -> X >= Max end),
    {X, ft_base:concat(L, R)}.

from_list(List) ->
    ft_base:from_list(new(), List).

max(Que) ->
    ft_base:measure(Que).

-module(ft_array). %% TODO: dequeue

-export([
         new/0,
         push_front/2,
         from_list/1,
         to_list/1,
         pop_front/1,
         pop_back/1,
         length/1,
         split_at/2,
         concat/2
        ]).

new() ->
    Empty   = 0,
    Append  = fun erlang:'+'/2,
    Measure = fun (_) -> 1 end,
    ft_base:new(ft_base:monoid(Empty, Append), Measure).

push_front(Ary, Elem) ->
    ft_base:push_l(Ary, Elem).

pop_front(Ary) ->
    ft_base:pop_l(Ary).

pop_back(Ary) ->
    ft_base:pop_r(Ary).

from_list(List) ->
    ft_base:from_list(new(), List).

to_list(Ary) ->
    ft_base:to_list(Ary).

length(Ary) ->
    ft_base:measure(Ary).

split_at(Ary, Index) ->
    ft_base:split(Ary, fun (X) -> Index < X end).

concat(Ary1, Ary2) ->
    ft_base:concat(Ary1, Ary2).

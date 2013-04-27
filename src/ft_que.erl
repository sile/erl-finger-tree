-module(ft_que).

-export([
         new/0,
         is_ft_que/1,
         is_empty/1,
         len/1,
         in/2, in_r/2,
         out/1, out_r/1,
         peek/1, peek_r/1,
         from_list/1,
         to_list/1,
         split/2,
         join/2,
         at/2
        ]).

-export_type([
              ft_que/0,
              item/0
             ]).

%%---------------------------------------------------------------------------------
%% Types
%%---------------------------------------------------------------------------------
-type ft_que() :: ft_base:ft_base(?MODULE).
-type item()   :: term().

%%---------------------------------------------------------------------------------
%% External Functions
%%---------------------------------------------------------------------------------
-spec new() -> ft_que().
new() ->
    Empty   = 0,
    Append  = fun erlang:'+'/2,
    Measure = fun (_) -> 1 end,
    ft_base:new(?MODULE, Empty, Append, Measure).

-spec is_ft_que(any()) -> boolean().
is_ft_que(Que) -> ft_base:is_finger_tree(Que, ?MODULE).

-spec is_empty(ft_que()) -> boolean().
is_empty(Que) -> ft_base:is_empty(Que).

-spec len(ft_que()) -> non_neg_integer().
len(Que) -> ft_base:measure(Que).

-spec in(item(), ft_que()) -> ft_que().
in(Item, Que) -> ft_base:push_r(Item, Que).

-spec peek(ft_que()) -> empty | {value, item()}.
peek(Que) ->
    case is_empty(Que) of
        true  -> empty;
        false -> {value, ft_base:head_l(Que)}
    end.

-spec out(ft_que()) -> {{value, item()}, ft_que()} | {empty, ft_que()}.
out(Que) ->
    case is_empty(Que) of
        true  ->
            {empty, Que};
        false ->
            {Item, Que2} = ft_base:pop_l(Que),
            {{value, Item}, Que2}
    end.

-spec in_r(item(), ft_que()) -> ft_que().
in_r(Item, Que) -> ft_base:push_l(Item, Que).

-spec out_r(ft_que()) -> {{value, item()}, ft_que()} | {empty, ft_que()}.
out_r(Que) ->
    case is_empty(Que) of
        true  ->
            {empty, Que};
        false ->
            {Item, Que2} = ft_base:pop_r(Que),
            {{value, Item}, Que2}
    end.

-spec peek_r(ft_que()) -> empty | {value, item()}.
peek_r(Que) ->
    case is_empty(Que) of
        true  -> empty;
        false -> {value, ft_base:head_r(Que)}
    end.

-spec from_list([item()]) -> ft_que().
from_list(Items) ->
    ft_base:append_list(new(), Items).

-spec to_list(ft_que()) -> [item()].
to_list(Que) -> ft_base:to_list(Que).

-spec join(ft_que(), ft_que()) -> ft_que().
join(Que1, Que2) ->
    ft_base:concat(Que1, Que2).

-spec split(non_neg_integer(), ft_que()) -> {ft_que(), ft_que()}.
split(N, Que) ->
    Len = len(Que),
    if
        N < 0   -> error(badarg);
        N > Len -> error(badarg);
        true    -> ft_base:split(fun (I) -> I > N end, Que)
    end.

-spec at(non_neg_integer(), ft_que()) -> item().
at(Index, Que) ->
    Len = len(Que),
    if
        Index < 0    -> error(badarg);
        Index >= Len -> error(badarg);
        true         ->
            {_, R} = ft_base:split(fun (I) -> I > Index end, Que),
            ft_base:head_l(R)
    end.    

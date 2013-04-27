-module(ft_ordseq).

-export([
         new/0,
         is_ft_ordseq/1,
         is_empty/1,
         len/1,
         in/2,
         out/1,
         out_r/1,
         from_list/1,
         to_list/1,
         delete/2
        ]).

-export_type([
              ft_ordseq/0,
              item/0
             ]).

%%---------------------------------------------------------------------------------
%% Types
%%---------------------------------------------------------------------------------
-type ft_ordseq() :: ft_base:ft_base(?MODULE).
-type item()      :: term().

%%---------------------------------------------------------------------------------
%% External Functions
%%---------------------------------------------------------------------------------
-spec new() -> ft_ordseq().
new() ->
    Empty   = make_ref(),
    Append  = fun (A, B) when B =:= Empty -> A;
                  (_, B)                  -> B
              end,
    Measure = fun (X) -> X end,
    ft_base:new(?MODULE, Empty, Append, Measure).

-spec is_ft_ordseq(any()) -> boolean().
is_ft_ordseq(Seq) -> ft_base:is_finger_tree(Seq, ?MODULE).

-spec is_empty(ft_ordseq()) -> boolean().
is_empty(Seq) -> ft_base:is_empty(Seq).

-spec len(ft_ordseq()) -> non_neg_integer().
len(Seq) -> ft_base:reduce_l(fun (_, Acc) -> Acc+1 end, 0, Seq).
    
-spec in(item(), ft_ordseq()) -> ft_ordseq().
in(Item, Seq) ->
    {L, R} = ft_base:split(fun (X) -> X >= Item end, Seq),
    ft_base:concat(L, ft_base:push_l(Item, R)).

-spec out(ft_ordseq()) -> {empty, ft_ordseq()} | {{value, item()}, ft_ordseq()}.
out(Seq) ->
    case is_empty(Seq) of
        true  -> {empty, Seq};
        false -> {Item, Que1} = ft_base:pop_l(Seq),
                 {{value, Item}, Que1}
    end.

-spec out_r(ft_ordseq()) -> {empty, ft_ordseq()} | {{value, item()}, ft_ordseq()}.
out_r(Seq) ->
    case is_empty(Seq) of
        true  -> {empty, Seq};
        false -> {Item, Que1} = ft_base:pop_r(Seq),
                 {{value, Item}, Que1}
    end.

-spec from_list([item()]) -> ft_ordseq().
from_list(Items) -> ft_base:append_list(new(), lists:sort(Items)).

-spec to_list(ft_ordseq()) -> [item()].
to_list(Seq) -> ft_base:to_list(Seq).

-spec delete(item(), ft_ordseq()) -> ft_ordseq().
delete(Item, Seq) ->
    {Less, EqualOrGreater} = ft_base:split(fun (X) -> X >= Item end, Seq),
    {_Equal, Greater}      = ft_base:split(fun (X) -> X >  Item end, EqualOrGreater),
    ft_base:concat(Less, Greater).

%% merge(Set1, Set2) ->
%%     case ft_base:is_empty(Set1) of
%%         true  -> Set2;
%%         false ->
%%             {Head1, Tail1} = pop(Set1),
%%             {L, R} = ft_base:split(Set2, fun (K) -> K > {key, Head1} end),
%%             ft_base:concat(L, ft_base:push_l(merge(Tail1, R), Head1))
%%     end.

%% split
%% take-until
%% drop-until

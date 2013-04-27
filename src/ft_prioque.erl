-module(ft_prioque).

-export([
         new/0,
         is_ft_prioque/1,
         is_empty/1,
         in/3,
         out/1,
         peek/1,
         from_list/1,
         to_list/1,
         join/2
        ]).

-export_type([
              ft_prioque/0,
              priority/0,
              item/0
             ]).

%%---------------------------------------------------------------------------------
%% Types
%%---------------------------------------------------------------------------------
-type ft_prioque() :: ft_base:ft_base(?MODULE).
-type priority()   :: term().
-type item()       :: term().

%%---------------------------------------------------------------------------------
%% External Functions
%%---------------------------------------------------------------------------------
-spec new() -> ft_prioque().
new() ->
    Empty   = monoid_empty(),
    Append  = fun monoid_max/2,
    Measure = fun (X) -> X end,
    ft_base:new(?MODULE, Empty, Append, Measure).

-spec is_ft_prioque(any()) -> boolean().
is_ft_prioque(Que) -> ft_base:is_finger_tree(Que, ?MODULE).

-spec is_empty(ft_prioque()) -> boolean().
is_empty(Que) -> ft_base:is_empty(Que).

-spec in(priority(), item(), ft_prioque()) -> ft_prioque().
in(Priority, Item, Que) ->
    ft_base:push_l({Priority, Item}, Que).

-spec out(ft_prioque()) -> {empty, ft_prioque()} | {{priority(), item()}, ft_prioque()}.
out(Que) ->
    case is_empty(Que) of
        true  -> {empty, Que};
        false ->
            Max = peek(Que),
            {Less, EqualOrGreater} = ft_base:split(fun (X) -> X >= Max end, Que),
            {_Eql, Greater}        = ft_base:pop_l(EqualOrGreater),
            {Max, ft_base:concat(Less, Greater)}
    end.

-spec peek(ft_prioque()) -> empty | {priority(), item()}.
peek(Que) ->
    case is_empty(Que) of
        true  -> empty;
        false -> ft_base:measure(Que)
    end.

-spec from_list([{priority(), item()}]) -> ft_prioque().
from_list(Entries) ->
    ft_base:append_list(new(), Entries).

-spec to_list(ft_prioque()) -> [{priority(), item()}].
to_list(Que) ->
    lists:sort(fun ({A,_}, {B,_}) -> A > B end,
               ft_base:to_list(Que)).

-spec join(ft_prioque(), ft_prioque()) -> ft_prioque().
join(Que1, Que2) ->
    ft_base:concat(Que1, Que2).

%%---------------------------------------------------------------------------------
%% Internal Functions
%%---------------------------------------------------------------------------------
-spec monoid_empty() -> ft_base:monoid_empty().
monoid_empty() -> minus_inf.

-spec monoid_max(ft_base:monoid_value(), ft_base:monoid_value()) -> ft_base:monoid_value().
monoid_max(minus_inf, A)       -> A;
monoid_max(A, minus_inf)       -> A;
monoid_max({P1,_}=A, {P2,_}=B) ->
    case P1 < P2 of
        true  -> B;
        false -> A
    end.

-module(ft_prioque).

-export([
         new/0
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
new() ->
    Empty   = monoid_empty(),
    Append  = fun monoid_max/2,
    Measure = fun (X) -> X end,
    ft_base:new(?MODULE, Empty, Append, Measure).

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

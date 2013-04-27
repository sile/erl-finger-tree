-module(ft_prioque_tests).
-include_lib("eunit/include/eunit.hrl").

new_test_() ->
    [
     {"new() が 適切なインスタンスを生成するか",
      fun () ->
              Que = ft_prioque:new(),
              ?assert(ft_prioque:is_ft_prioque(Que))
      end}
    ].

is_ft_prioque_test() ->
    ?assertEqual(true,  ft_prioque:is_ft_prioque(ft_prioque:new())),
    ?assertEqual(false, ft_prioque:is_ft_prioque(not_a_ft_prioque)).

is_empty_test_() ->
    [
     {"空の場合は is_empty() は true を返す",
      fun () ->
              Que = ft_prioque:new(),
              ?assert(ft_prioque:is_empty(Que))
      end},
     {"空ではない場合は is_empty() は false を返す",
      fun () ->
              Que0 = ft_prioque:new(),
              Que1 = ft_prioque:in(1, item, Que0),
              ?assertEqual(false, ft_prioque:is_empty(Que1))
      end}
    ].

in_out_test_() ->
    [
     {"空のキューに out() を適用した場合",
      fun () ->
              Que = ft_prioque:new(),
              ?assertMatch({empty, _}, ft_prioque:out(Que))
      end},
     {"in() で格納した要素は 優先順位が高い順に out() で取り出される",
      fun () ->
              Que0 = ft_prioque:new(),
              Que1 = ft_prioque:in(10, a, ft_prioque:in(1, b, ft_prioque:in(5, c, Que0))),

              {Rlt2, Que2} = ft_prioque:out(Que1),
              ?assertEqual({10, a}, Rlt2),

              {Rlt3, Que3} = ft_prioque:out(Que2),
              ?assertEqual({5, c}, Rlt3),

              {Rlt4,_Que4} = ft_prioque:out(Que3),
              ?assertEqual({1, b}, Rlt4)
      end}
    ].

peek_test_() ->
    [
     {"空のキューに peek() を適用した結果",
      fun () ->
              Que = ft_prioque:new(),
              ?assertEqual(empty, ft_prioque:peek(Que))
      end},
     {"peek() で取得されるのは、一番優先順位が高い要素",
      fun () ->
              Que0 = ft_prioque:new(),
              Que1 = ft_prioque:in(10, a, ft_prioque:in(1, b, ft_prioque:in(5, c, Que0))),
              ?assertEqual({10, a}, ft_prioque:peek(Que1))
      end}
    ].

from_list_test_() ->
    [
     {"from_list() を空リストに適用した場合",
      fun () ->
              Que = ft_prioque:from_list([]),
              ?assert(ft_prioque:is_empty(Que))
      end},
     {"from_list() で作成したキューから要素を取り出す",
      fun () ->
              List = [{10, a},
                      {1,  b},
                      {4,  c},
                      {0,  d},
                      {33, e}],
              Que1 = ft_prioque:from_list(List),

              {Rlt2, Que2} = ft_prioque:out(Que1),
              ?assertEqual({33, e}, Rlt2),

              {Rlt3, Que3} = ft_prioque:out(Que2),
              ?assertEqual({10, a}, Rlt3),

              {Rlt4, Que4} = ft_prioque:out(Que3),
              ?assertEqual({4, c}, Rlt4),

              {Rlt5, Que5} = ft_prioque:out(Que4),
              ?assertEqual({1, b}, Rlt5),

              {Rlt6,_Que6} = ft_prioque:out(Que5),
              ?assertEqual({0, d}, Rlt6)
      end}
    ].

to_list_test_() ->
    [
     {"to_list() を空キューに適用した場合",
      fun () ->
              Que = ft_prioque:new(),
              ?assertEqual([], ft_prioque:to_list(Que))
      end},
     {"to_list() の結果は優先順位の高い順にソートされている",
      fun () ->
              Que0 = ft_prioque:new(),
              Que1 = ft_prioque:in(10, a, ft_prioque:in(1, b, ft_prioque:in(5, c, Que0))),
              
              ?assertEqual([{10, a}, {5, c}, {1, b}], ft_prioque:to_list(Que1))
      end}
    ].

join_test_() ->
    [
     {"二つのキューを結合する",
      fun () ->
              List1 = [{10, a}, {1,  b}, {4,  c}, {0,  d}, {33, e}],
              List2 = [{7, z}, {99,  y}, {x,  20}],
              Que1 = ft_prioque:from_list(List1),
              Que2 = ft_prioque:from_list(List2),

              Que3 = ft_prioque:join(Que1, Que2),
              ?assertEqual(lists:reverse(lists:sort(List1++List2)), 
                           ft_prioque:to_list(Que3))
      end}
    ].

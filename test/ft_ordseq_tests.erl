-module(ft_ordseq_tests).
-include_lib("eunit/include/eunit.hrl").

new_test_() ->
    [
     {"new() が 適切なインスタンスを生成するか",
      fun () ->
              Seq = ft_ordseq:new(),
              ?assert(ft_ordseq:is_ft_ordseq(Seq)),
              ?assertEqual(0, ft_ordseq:len(Seq))
      end}
    ].

is_ft_ordseq_test() ->
    ?assertEqual(true,  ft_ordseq:is_ft_ordseq(ft_ordseq:new())),
    ?assertEqual(false, ft_ordseq:is_ft_ordseq(not_a_ft_ordseq)).

is_empty_test_() ->
    [
     {"空の場合は is_empty() は true を返す",
      fun () ->
              Seq = ft_ordseq:new(),
              ?assert(ft_ordseq:is_empty(Seq))
      end},
     {"空ではない場合は is_empty() は false を返す",
      fun () ->
              Seq0 = ft_ordseq:new(),
              Seq1 = ft_ordseq:in(item, Seq0),
              ?assertEqual(false, ft_ordseq:is_empty(Seq1))
      end}
    ].

in_out_test_() ->
    [
     {"空の列に out() および out_r() を適用した場合",
      fun () ->
              Seq = ft_ordseq:new(),
              ?assertMatch({empty, _}, ft_ordseq:out(Seq)),
              ?assertMatch({empty, _}, ft_ordseq:out_r(Seq))
      end},
     {"in() で格納した要素は out() で昇順に取り出せる",
      fun () ->
              Seq0 = ft_ordseq:new(),
              Seq1 = ft_ordseq:in(m, ft_ordseq:in(x, ft_ordseq:in(a, Seq0))),

              {Rlt2, Seq2} = ft_ordseq:out(Seq1),
              ?assertMatch({value, a}, Rlt2),

              {Rlt3, Seq3} = ft_ordseq:out(Seq2),
              ?assertMatch({value, m}, Rlt3),

              {Rlt4,_Seq4} = ft_ordseq:out(Seq3),
              ?assertMatch({value, x}, Rlt4)
      end},
     {"in() で格納した要素は out_r() で降順に取り出せる",
      fun () ->
              Seq0 = ft_ordseq:new(),
              Seq1 = ft_ordseq:in(m, ft_ordseq:in(x, ft_ordseq:in(a, Seq0))),

              {Rlt2, Seq2} = ft_ordseq:out_r(Seq1),
              ?assertMatch({value, x}, Rlt2),

              {Rlt3, Seq3} = ft_ordseq:out_r(Seq2),
              ?assertMatch({value, m}, Rlt3),

              {Rlt4,_Seq4} = ft_ordseq:out_r(Seq3),
              ?assertMatch({value, a}, Rlt4)
      end}
    ].

from_list_test_() ->
    [
     {"from_list() を空リストに適用した場合",
      fun () ->
              Seq = ft_ordseq:from_list([]),
              ?assert(ft_ordseq:is_empty(Seq))
      end},
     {"from_list() で作成した列から要素を取り出す",
      fun () ->
              List = [c,a,x,l],
              Seq = ft_ordseq:from_list(List),

              ?assertEqual(length(List), ft_ordseq:len(Seq)),
              
              {Rlt1, Seq1} = ft_ordseq:out(Seq),
              ?assertMatch({value, a}, Rlt1),

              {Rlt2, Seq2} = ft_ordseq:out(Seq1),
              ?assertMatch({value, c}, Rlt2),

              {Rlt3, Seq3} = ft_ordseq:out(Seq2),
              ?assertMatch({value, l}, Rlt3),

              {Rlt4,_Seq4} = ft_ordseq:out(Seq3),
              ?assertMatch({value, x}, Rlt4)
      end}
    ].

to_list_test_() ->
    [
     {"to_list() を空キューに適用した場合",
      fun () ->
              Seq = ft_ordseq:new(),
              ?assertEqual([], ft_ordseq:to_list(Seq))
      end},
     {"in() で格納した要素を to_list() でリストにする",
      fun () ->
              Seq0 = ft_ordseq:new(),
              Seq1 = ft_ordseq:in(c, ft_ordseq:in(b, ft_ordseq:in(a, Seq0))),

              ?assertEqual([a,b,c], ft_ordseq:to_list(Seq1))
      end}
    ].

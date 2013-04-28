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

peek_test_() ->
    [
     {"peek() および peek_r() を空列に適用した場合",
      fun () ->
              Seq = ft_ordseq:new(),
              ?assertEqual(empty, ft_ordseq:peek(Seq)),
              ?assertEqual(empty, ft_ordseq:peek_r(Seq))
      end},
     {"peek() および peek_r() で列の端の要素を取得する",
      fun () ->
              Seq = ft_ordseq:from_list([e,r,l,a,n,g]),
              ?assertEqual({value, a}, ft_ordseq:peek(Seq)),
              ?assertEqual({value, r}, ft_ordseq:peek_r(Seq))
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
      end},
     {"要素の重複が可能",
      fun () ->
              List = [a,a,a],
              Seq = ft_ordseq:from_list(List),
              ?assertEqual(List, ft_ordseq:to_list(Seq))
      end}
    ].

to_list_test_() ->
    [
     {"to_list() を空の列に適用した場合",
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

delete_test_() ->
    [
     {"delete() を空の列に適用した場合",
      fun () ->
              Seq0 = ft_ordseq:new(),
              Seq1 = ft_ordseq:delete(item, Seq0),
              ?assert(ft_ordseq:is_empty(Seq1))
      end},
     {"delete() で要素を削除する",
      fun () ->
              Seq0 = ft_ordseq:from_list([a,b,c]),
              Seq1 = ft_ordseq:delete(b, Seq0),
              ?assertEqual([a,c], ft_ordseq:to_list(Seq1))
      end},
     {"delete() では一致する要素全てが削除される",
      fun () ->
              Seq0 = ft_ordseq:from_list([x,a,y,a,b,a,c]),
              Seq1 = ft_ordseq:delete(a, Seq0),
              ?assertEqual([b,c,x,y], ft_ordseq:to_list(Seq1))
      end}
    ].

member_test_() ->
    [
     {"要素が存在するかどうかの判定",
      fun () ->
              Seq = ft_ordseq:from_list([e,r,l,a,n,g]),
              ?assertEqual(true,  ft_ordseq:member(n, Seq)),
              ?assertEqual(false, ft_ordseq:member(b, Seq)),
              ?assertEqual(false, ft_ordseq:member(z, Seq))
      end}
    ].

merge_test_() ->
    [
     {"空列とのマージ",
      fun () ->
              Empty = ft_ordseq:new(),
              Seq   = ft_ordseq:from_list([a,b,c]),
              
              ?assertEqual(0, ft_ordseq:len(ft_ordseq:merge(Empty, Empty))),
              ?assertEqual(3, ft_ordseq:len(ft_ordseq:merge(Seq, Empty))),
              ?assertEqual(3, ft_ordseq:len(ft_ordseq:merge(Empty, Seq)))
      end},
     {"列同士のマージ",
      fun () ->
              Seq0 = ft_ordseq:from_list([l,i,s,p]),
              Seq1 = ft_ordseq:from_list([e,r,l,a,n,g]),
              Seq2 = ft_ordseq:merge(Seq0, Seq1),
              ?assertEqual([a,e,g,i,l,l,n,p,r,s], ft_ordseq:to_list(Seq2))
      end}
    ].

split_test_() ->
    [
     {"空の列の分割",
      fun () ->
              Seq = ft_ordseq:new(),
              {SeqL, SeqR} = ft_ordseq:split(j, Seq),
              ?assert(ft_ordseq:is_empty(SeqL)),
              ?assert(ft_ordseq:is_empty(SeqR))
      end},
     {"列の分割",
      fun () ->
              Seq = ft_ordseq:from_list([e,r,l,a,n,g]),
              {SeqL, SeqR} = ft_ordseq:split(i, Seq),
              ?assertEqual([a,e,g], ft_ordseq:to_list(SeqL)),
              ?assertEqual([l,n,r], ft_ordseq:to_list(SeqR))
      end},
     {"分割に指定した要素が、列に存在する場合",
      fun () ->
              Seq = ft_ordseq:from_list([e,r,l,a,n,g]),
              {SeqL, SeqR} = ft_ordseq:split(l, Seq),
              ?assertEqual([a,e,g], ft_ordseq:to_list(SeqL)),
              ?assertEqual([l,n,r], ft_ordseq:to_list(SeqR))
      end}
    ].

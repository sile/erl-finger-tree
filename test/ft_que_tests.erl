-module(ft_que_tests).
-include_lib("eunit/include/eunit.hrl").

new_test_() ->
    [
     {"ft_que:new() が 適切なインスタンスを生成するか",
      fun () ->
              Que = ft_que:new(),
              ?assert(ft_que:is_ft_que(Que))
      end}
    ].

is_ft_que_test() ->
    ?assertEqual(true,  ft_que:is_ft_que(ft_que:new())),
    ?assertEqual(false, ft_que:is_ft_que(not_a_ft_que)).

is_empty_test_() ->
    [
     {"空の場合は is_empty() は true を返す",
      fun () ->
              Que = ft_que:new(),
              ?assert(ft_que:is_empty(Que))
      end},
     {"空ではない場合は is_empty() は false を返す",
      fun () ->
              Que0 = ft_que:new(),
              Que1 = ft_que:in(item, Que0),
              ?assertEqual(false, ft_que:is_empty(Que1))
      end}
    ].

len_test_() ->
    [
     {"len() は格納されている要素のサイズを返す",
      fun () ->
              Que0 = ft_que:new(),
              ?assertEqual(0, ft_que:len(Que0)),

              Que1 = ft_que:in(item, Que0),
              ?assertEqual(1, ft_que:len(Que1)),
              
              Que2 = ft_que:in(item, Que1),
              ?assertEqual(2, ft_que:len(Que2))
      end}
    ].

in_out_test_() ->
    [
     {"空のキューに out() を適用した場合",
      fun () ->
              Que0 = ft_que:new(),
              ?assertMatch({empty, _Que}, ft_que:out(Que0))
      end},
     {"空のキューに out_r() を適用した場合",
      fun () ->
              Que0 = ft_que:new(),
              ?assertMatch({empty, _Que}, ft_que:out_r(Que0))
      end},
     {"in() で格納した要素が out() で取り出せる",
      fun () ->
              Que0 = ft_que:new(),
              Que1 = ft_que:in(item, Que0),

              {Rlt2, Que2} = ft_que:out(Que1),
              ?assertMatch({value, item}, Rlt2),              
              
              ?assert(ft_que:is_empty(Que2))
      end},
     {"in_r() で格納した要素が out_r() で取り出せる",
      fun () ->
              Que0 = ft_que:new(),
              Que1 = ft_que:in_r(item, Que0),
              
              {Rlt2, Que2} = ft_que:out_r(Que1),
              ?assertMatch({value, item}, Rlt2),
              
              ?assert(ft_que:is_empty(Que2))
      end},
     {"in() と out() は FIFO の関係",
      fun () ->
              Que0 = ft_que:new(),
              Que1 = ft_que:in(item2, ft_que:in(item1, Que0)),
              
              {Rlt2, Que2} = ft_que:out(Que1),
              ?assertMatch({value, item1}, Rlt2),

              {Rlt3,_Que3} = ft_que:out(Que2),
              ?assertMatch({value, item2}, Rlt3)
      end},
     {"in_r() と out_r() は FIFO の関係",
      fun () ->
              Que0 = ft_que:new(),
              Que1 = ft_que:in_r(item2, ft_que:in_r(item1, Que0)),
              
              {Rlt2, Que2} = ft_que:out_r(Que1),
              ?assertMatch({value, item1}, Rlt2),

              {Rlt3,_Que3} = ft_que:out_r(Que2),
              ?assertMatch({value, item2}, Rlt3)
      end},
     {"in() と out_r() は FILO の関係",
      fun () ->
              Que0 = ft_que:new(),
              Que1 = ft_que:in(item2, ft_que:in(item1, Que0)),
              
              {Rlt2, Que2} = ft_que:out_r(Que1),
              ?assertMatch({value, item2}, Rlt2),
              
              {Rlt3,_Que3} = ft_que:out_r(Que2),
              ?assertMatch({value, item1}, Rlt3)
      end},
     {"in_r() と out() は FILO の関係",
      fun () ->
              Que0 = ft_que:new(),
              Que1 = ft_que:in_r(item2, ft_que:in_r(item1, Que0)),
              
              {Rlt2, Que2} = ft_que:out(Que1),
              ?assertMatch({value, item2}, Rlt2),
              
              {Rlt3,_Que3} = ft_que:out(Que2),
              ?assertMatch({value, item1}, Rlt3)
      end}
    ].

head_test_() ->
    [
     {"空のキューの peek() および peek_r() を適用した場合",
      fun () ->
              Que = ft_que:new(),
              ?assertMatch(empty, ft_que:peek(Que)),
              ?assertMatch(empty, ft_que:peek_r(Que))
      end},
     {"peek() および peek_r() で端の要素を取得する",
      fun () ->
              Que = ft_que:from_list([a,b,c]),
              ?assertMatch({value, a}, ft_que:peek(Que)),
              ?assertMatch({value, c}, ft_que:peek_r(Que))
      end}
    ].

from_list_test_() ->
    [
     {"from_list() を空リストに適用した場合",
      fun () ->
              Que = ft_que:from_list([]),
              ?assert(ft_que:is_empty(Que))
      end},
     {"from_list() で作成したキューのサイズは、元のリストのサイズと等しい",
      fun () ->
              List = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
              Que = ft_que:from_list(List),
              ?assertEqual(length(List), ft_que:len(Que))
      end},
     {"from_list() で作成したキューから要素を取り出す",
      fun () ->
              List = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
              Que = ft_que:from_list(List),
              
              {Rlt1, Que1} = ft_que:out(Que),
              ?assertMatch({value, a}, Rlt1),

              {Rlt2, Que2} = ft_que:out(Que1),
              ?assertMatch({value, b}, Rlt2),

              {Rlt3, Que3} = ft_que:out_r(Que2),
              ?assertMatch({value, z}, Rlt3),

              {Rlt4,_Que4} = ft_que:out_r(Que3),
              ?assertMatch({value, y}, Rlt4)
      end}
    ].

to_list_test_() ->
    [
     {"to_list() を空キューに適用した場合",
      fun () ->
              Que = ft_que:new(),
              ?assertEqual([], ft_que:to_list(Que))
      end},
     {"in() で格納した要素を to_list() でリストにする",
      fun () ->
              Que0 = ft_que:new(),
              Que1 = ft_que:in(c, ft_que:in(b, ft_que:in(a, Que0))),

              ?assertEqual([a,b,c], ft_que:to_list(Que1))
      end},
     {"in_r() で格納した要素を to_list() でリストにする",
      fun () ->
              Que0 = ft_que:new(),
              Que1 = ft_que:in_r(c, ft_que:in_r(b, ft_que:in_r(a, Que0))),
              
              ?assertEqual([c,b,a], ft_que:to_list(Que1))
      end},
     {"List == to_list(from_list(List))",
      fun () ->
              List = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
              Que = ft_que:from_list(List),
              
              ?assertEqual(List, ft_que:to_list(Que))
      end}
    ].

split_test_() ->
    [
     {"空のキューに split() を適用した場合",
      fun () ->
              Que = ft_que:new(),
              {QueL, QueR} = ft_que:split(0, Que),

              ?assert(ft_que:is_empty(QueL)),
              ?assert(ft_que:is_empty(QueR))
      end},
     {"キューの真ん中で分割",
      fun () ->
              List = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
              Que = ft_que:from_list(List),
              
              Pos = length(List) div 2,
              {QueL, QueR} = ft_que:split(Pos, Que),
              {ListL, ListR} = lists:split(Pos, List),
              
              ?assertEqual(ListL, ft_que:to_list(QueL)),
              ?assertEqual(ListR, ft_que:to_list(QueR))
      end},
     {"キューの右端で分割",
      fun () ->
              List = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
              Que = ft_que:from_list(List),

              Pos = 0,
              {QueL, QueR} = ft_que:split(Pos, Que),
              
              ?assertEqual([],   ft_que:to_list(QueL)),
              ?assertEqual(List, ft_que:to_list(QueR))
      end},
     {"キューの左端で分割",
      fun () ->
              List = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
              Que = ft_que:from_list(List),

              Pos = length(List),
              {QueL, QueR} = ft_que:split(Pos, Que),
              
              ?assertEqual(List, ft_que:to_list(QueL)),
              ?assertEqual([],   ft_que:to_list(QueR))              
      end},
     {"分割位置に負数を指定した場合",
      fun () ->
              List = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
              Que = ft_que:from_list(List),
              Pos = -1,
              ?assertError(badarg, ft_que:split(Pos, Que))
      end},
     {"分割位置がキューのサイズを超えている場合",
      fun () ->
              List = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
              Que = ft_que:from_list(List),
              Pos = length(List) + 1,
              ?assertError(badarg, ft_que:split(Pos, Que))
      end}
    ].

join_test_() ->
    [
     {"二つのキューを結合する",
      fun () ->
              List1 = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
              List2 = [0,1,2,3,4,5,6,7,9],
              Que1 = ft_que:from_list(List1),
              Que2 = ft_que:from_list(List2),

              Que3 = ft_que:join(Que1, Que2),
              ?assertEqual(List1++List2, ft_que:to_list(Que3))
      end}
    ].

at_test_() ->
    [
     {"添字指定での要素アクセス",
      fun () ->
              List = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
              Que = ft_que:from_list(List),

              ?assertEqual(a, ft_que:at(0, Que)),
              ?assertEqual(k, ft_que:at(10, Que)),
              ?assertEqual(z, ft_que:at(25, Que))
      end},
     {"範囲外アクセス",
      fun () ->
              List = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
              Que = ft_que:from_list(List),

              ?assertError(badarg, ft_que:at(-1, Que)),
              ?assertError(badarg, ft_que:at(length(List), Que))
      end}
    ].

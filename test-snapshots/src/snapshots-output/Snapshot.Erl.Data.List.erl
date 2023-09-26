-module(snapshot_erl_data_list@ps).
-export([unconsA1/0, unconsA0/0, uncons2/0, uncons1/0, uncons0/1, uncons0/0, n/0, lit2/0, lit1/0, lit0/0, hd/0, concatSimple/2, concatSimple/0, concatNeut/0, concatLR/0, result/0, concatL/1, concatL/0, concat/0]).
unconsA1() ->
  {just,#{head => 1,
  tail => []}}.
unconsA0() ->
  {nothing}.
uncons2() ->
  {nothing}.
uncons1() ->
  {just,#{head => 1,
  tail => [2,3]}}.
uncons0(L) ->
  ((erl_data_list_types@ps:uncons())((L ++ L))).
uncons0() ->
  (fun
    (L) ->
      ((erl_data_list_types@ps:uncons())((L ++ L)))
  end).
n() ->
  [].
lit2() ->
  [4,5,6].
lit1() ->
  [1,2,3].
lit0() ->
  [].
hd() ->
  {just,1}.
concatSimple(L, R@1) ->
  ([1,2|L] ++ [3|R@1]).
concatSimple() ->
  (fun
    (L) ->
      (fun
        (R) ->
          ([1,2|L] ++ [3|R])
      end)
  end).
concatNeut() ->
  [1,2,3|(snapshot_erl_data_list@ps:lit2())].
concatLR() ->
  [1,2,3,4].
result() ->
  #{hd => {just,1},
  concatNeut => [1,2,3|(snapshot_erl_data_list@ps:lit2())],
  concatLR => [1,2,3,4]}.
concatL(L) ->
  [1,2,3|L].
concatL() ->
  (fun
    (L) ->
      [1,2,3|L]
  end).
concat() ->
  [1,2,3,4,5,6].

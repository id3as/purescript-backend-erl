-module(snapshot_erl_data_list@ps).
-export([unconsA1/0, unconsA0/0, uncons2/0, uncons1/0, uncons0/0, uncons0/1, n/0, lit2/0, lit1/0, lit0/0, hd/0, concatSimple/0, concatSimple/2, concatNeut/0, concatLR/0, result/0, concatL/0, concatL/1, concat/0]).
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
uncons0() ->
  (fun
    (V@0) ->
      (uncons0(V@0))
  end).
uncons0(L) ->
  ((erl_data_list_types@ps:uncons())((L ++ L))).
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
concatSimple() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (concatSimple(V@0, V@1))
      end)
  end).
concatSimple(L, R) ->
  ([1,2|L] ++ [3|R]).
concatNeut() ->
  [1,2,3|(snapshot_erl_data_list@ps:lit2())].
concatLR() ->
  [1,2,3,4].
result() ->
  #{hd => {just,1},
  concatNeut => [1,2,3|(snapshot_erl_data_list@ps:lit2())],
  concatLR => [1,2,3,4]}.
concatL() ->
  (fun
    (V@0) ->
      (concatL(V@0))
  end).
concatL(L) ->
  [1,2,3|L].
concat() ->
  [1,2,3,4,5,6].

-module(snapshot_erl_data_list@ps).
-export([unconsA1/0, unconsA0/0, uncons2/0, uncons1/0, uncons0/0, n/0, lit2/0, lit1/0, lit0/0, hd/0, concatSimple/0, concatNeut/0, concatLR/0, result/0, concatL/0, concat/0]).
unconsA1() ->
  {just,#{head => 1,
  tail => []}}.
unconsA0() ->
  (data_maybe@ps:'Nothing'()).
uncons2() ->
  (data_maybe@ps:'Nothing'()).
uncons1() ->
  {just,#{head => 1,
  tail => [2,3]}}.
uncons0() ->
  (fun
    (L@0) ->
      ((erl_data_list_types@ps:uncons())((L@0 ++ L@0)))
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
  begin
    V@0 = ((erl_data_list_types@ps:uncons())((lit1()))),
    case (just =:= (erlang:element(1, V@0))) of
      true ->
        {just,(maps:get(head, (erlang:element(2, V@0))))};
      _ ->
        (data_maybe@ps:'Nothing'())
    end
  end.
concatSimple() ->
  (fun
    (L@0) ->
      (fun
        (R@1) ->
          ([1,2|L@0] ++ [3|R@1])
      end)
  end).
concatNeut() ->
  ((lit1()) ++ (lit2())).
concatLR() ->
  [1,2,3,4].
result() ->
  #{hd => (hd()),
  concatNeut => (concatNeut()),
  concatLR => (concatLR())}.
concatL() ->
  (fun
    (L@0) ->
      [1,2,3|L@0]
  end).
concat() ->
  [1,2,3,4,5,6].

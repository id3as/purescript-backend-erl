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
  begin
    V = ((erl_data_list_types@ps:uncons())((snapshot_erl_data_list@ps:lit1()))),
    case (just =:= (erlang:element(1, V))) of
      true ->
        {just,(maps:get(head, (erlang:element(2, V))))};
      _ ->
        (data_maybe@ps:'Nothing'())
    end
  end.
concatSimple() ->
  (fun
    (L) ->
      (fun
        (R) ->
          ([1,2|L] ++ [3|R])
      end)
  end).
concatNeut() ->
  ((snapshot_erl_data_list@ps:lit1()) ++ (snapshot_erl_data_list@ps:lit2())).
concatLR() ->
  [1,2,3,4].
result() ->
  #{hd => (snapshot_erl_data_list@ps:hd()),
  concatNeut => (snapshot_erl_data_list@ps:concatNeut()),
  concatLR => (snapshot_erl_data_list@ps:concatLR())}.
concatL() ->
  (fun
    (L) ->
      [1,2,3|L]
  end).
concat() ->
  [1,2,3,4,5,6].

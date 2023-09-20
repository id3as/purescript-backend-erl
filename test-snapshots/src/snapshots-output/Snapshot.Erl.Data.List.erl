-module(snapshot_erl_data_list@ps).
-export([n/0, lit2/0, lit1/0, hd/0, concatLR/0, concatL/0, concat/0, result/0]).
n() ->
  [].
lit2() ->
  [4,5,6].
lit1() ->
  [1,2,3].
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
concatLR() ->
  [1,2,3,4].
concatL() ->
  (fun
    (L@0) ->
      [1,2,3|L@0]
  end).
concat() ->
  (((erl_data_list_types@ps:appendImpl())((lit1())))((lit2()))).
result() ->
  #{hd => (hd()),
  concat => (concat()),
  concatLR => (concatLR())}.

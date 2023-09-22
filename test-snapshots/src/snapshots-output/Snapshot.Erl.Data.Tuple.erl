-module(snapshot_erl_data_tuple@ps).
-export([uncurriedMore/0, uncurried/0, tf2/0, t5/0, t2/0, sndt2/0, r5/0, result/0, fsttf2/0, fstt2/0]).
uncurriedMore() ->
  (fun
    ({A@0,B@1,C@2,D@3,E@4}) ->
      (fun
        (F@5) ->
          (fun
            ({G@6,H@7}) ->
              {((A@0 + B@1) + G@6),<<C@2/binary, D@3/binary, (F@5(E@4))/binary, H@7/binary>>}
          end)
      end)
  end).
uncurried() ->
  (fun
    ({A@0,B@1,C@2,D@3,_}) ->
      {(A@0 + B@1),<<C@2/binary, D@3/binary>>}
  end).
tf2() ->
  {(fun
    (I@0) ->
      (I@0 - 5)
  end),<<"7">>}.
t5() ->
  {3,4,<<"hi">>,<<"there">>,$V}.
t2() ->
  {4,<<"hi">>}.
sndt2() ->
  <<"hi">>.
r5() ->
  {7,<<"hithere">>}.
result() ->
  ((((snapshot_erl_data_tuple@ps:uncurriedMore())({3,4,<<"hi">>,<<"there">>,$V}))((fun
    (_) ->
      <<"">>
  end)))((snapshot_erl_data_tuple@ps:r5()))).
fsttf2() ->
  7.
fstt2() ->
  4.

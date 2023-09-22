-module(snapshot_primOps@ps).
-export([stringOps/0, numberOps/0, intOps/0, booleanOps/0]).
stringOps() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([<<X/binary, Y/binary>>]))
      end)
  end).
numberOps() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([(X + Y),(X / Y),(X * Y),(X - Y)]))
      end)
  end).
intOps() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([(X band Y),(X bor Y),(X bsr Y),(X bsl Y),(X bsr Y),(X xor Y),(X + Y),(X div Y),(X * Y),(X - Y)]))
      end)
  end).
booleanOps() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([(X andalso Y),(X orelse Y)]))
      end)
  end).

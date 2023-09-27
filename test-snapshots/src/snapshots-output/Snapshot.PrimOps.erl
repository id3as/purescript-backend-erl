-module(snapshot_primOps@ps).
-export([stringOps/0, stringOps/2, numberOps/0, numberOps/2, intOps/0, intOps/2, booleanOps/0, booleanOps/2]).
-compile(no_auto_import).
stringOps() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (stringOps(V@0, V@1))
      end)
  end).
stringOps(X, Y) ->
  (array:from_list([<<X/binary, Y/binary>>])).
numberOps() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (numberOps(V@0, V@1))
      end)
  end).
numberOps(X, Y) ->
  (array:from_list([(X + Y),(X / Y),(X * Y),(X - Y)])).
intOps() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (intOps(V@0, V@1))
      end)
  end).
intOps(X, Y) ->
  (array:from_list([(X band Y),(X bor Y),(X bsr Y),(X bsl Y),(X bsr Y),(X xor Y),(X + Y),(X div Y),(X * Y),(X - Y)])).
booleanOps() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (booleanOps(V@0, V@1))
      end)
  end).
booleanOps(X, Y) ->
  (array:from_list([(X andalso Y),(X orelse Y)])).

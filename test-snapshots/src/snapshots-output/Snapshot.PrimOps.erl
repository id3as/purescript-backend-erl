-module(snapshot_primOps@ps).
-export([stringOps/2, stringOps/0, numberOps/2, numberOps/0, intOps/2, intOps/0, booleanOps/2, booleanOps/0]).
stringOps(X, Y@1) ->
  (array:from_list([<<X/binary, Y@1/binary>>])).
stringOps() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([<<X/binary, Y/binary>>]))
      end)
  end).
numberOps(X, Y@1) ->
  (array:from_list([(X + Y@1),(X / Y@1),(X * Y@1),(X - Y@1)])).
numberOps() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([(X + Y),(X / Y),(X * Y),(X - Y)]))
      end)
  end).
intOps(X, Y@1) ->
  (array:from_list([(X band Y@1),(X bor Y@1),(X bsr Y@1),(X bsl Y@1),(X bsr Y@1),(X xor Y@1),(X + Y@1),(X div Y@1),(X * Y@1),(X - Y@1)])).
intOps() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([(X band Y),(X bor Y),(X bsr Y),(X bsl Y),(X bsr Y),(X xor Y),(X + Y),(X div Y),(X * Y),(X - Y)]))
      end)
  end).
booleanOps(X, Y@1) ->
  (array:from_list([(X andalso Y@1),(X orelse Y@1)])).
booleanOps() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([(X andalso Y),(X orelse Y)]))
      end)
  end).

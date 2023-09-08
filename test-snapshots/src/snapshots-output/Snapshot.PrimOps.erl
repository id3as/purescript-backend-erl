-module(snapshot_primOps).
-compile(export_all).
stringOps() -> 
  (fun
    (X@0) ->
      (fun
        (Y@1) ->
          (array:from_list([(unicode:characters_to_binary([X@0,Y@1], utf8))]))
      end)
  end).
numberOps() -> 
  (fun
    (X@0) ->
      (fun
        (Y@1) ->
          (array:from_list([(X@0 + Y@1),(X@0 / Y@1),(X@0 * Y@1),(X@0 - Y@1)]))
      end)
  end).
intOps() -> 
  (fun
    (X@0) ->
      (fun
        (Y@1) ->
          (array:from_list([(X@0 band Y@1),(X@0 bor Y@1),(X@0 bsr Y@1),(X@0 bsl Y@1),(X@0 bsr Y@1),(X@0 xor Y@1),(X@0 + Y@1),(((data_euclideanRing:intDiv())(X@0))(Y@1)),(X@0 * Y@1),(X@0 - Y@1)]))
      end)
  end).
booleanOps() -> 
  (fun
    (X@0) ->
      (fun
        (Y@1) ->
          (array:from_list([(X@0 andalso Y@1),(X@0 orelse Y@1)]))
      end)
  end).

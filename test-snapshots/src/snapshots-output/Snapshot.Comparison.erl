-module('Snapshot.Comparison').
-compile(export_all).
stringComparison() -> 
  (fun
    (X@0) ->
      (fun
        (Y@1) ->
          (array:from_list([(X@0 =:= Y@1),(X@0 =/= Y@1),(X@0 < Y@1),(X@0 =< Y@1),(X@0 >= Y@1),(X@0 > Y@1)]))
      end)
  end).
numberComparison() -> 
  (fun
    (X@0) ->
      (fun
        (Y@1) ->
          (array:from_list([(X@0 =:= Y@1),(X@0 =/= Y@1),(X@0 < Y@1),(X@0 =< Y@1),(X@0 >= Y@1),(X@0 > Y@1)]))
      end)
  end).
integerComparison() -> 
  (fun
    (X@0) ->
      (fun
        (Y@1) ->
          (array:from_list([(X@0 =:= Y@1),(X@0 =/= Y@1),(X@0 < Y@1),(X@0 =< Y@1),(X@0 >= Y@1),(X@0 > Y@1)]))
      end)
  end).
charComparison() -> 
  (fun
    (X@0) ->
      (fun
        (Y@1) ->
          (array:from_list([(X@0 =:= Y@1),(X@0 =/= Y@1),(X@0 < Y@1),(X@0 =< Y@1),(X@0 >= Y@1),(X@0 > Y@1)]))
      end)
  end).
booleanComparison() -> 
  (fun
    (X@0) ->
      (fun
        (Y@1) ->
          (array:from_list([(X@0 =:= Y@1),(X@0 =/= Y@1),(X@0 < Y@1),(X@0 =< Y@1),(X@0 >= Y@1),(X@0 > Y@1)]))
      end)
  end).

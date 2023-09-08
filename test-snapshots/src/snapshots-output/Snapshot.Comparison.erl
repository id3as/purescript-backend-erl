-module(snapshot_comparison@ps).
-export([stringComparison/0, numberComparison/0, integerComparison/0, charComparison/0, booleanComparison/0]).
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

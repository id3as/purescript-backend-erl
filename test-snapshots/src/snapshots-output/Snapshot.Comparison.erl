-module(snapshot_comparison@ps).
-export([stringComparison/2, stringComparison/0, numberComparison/2, numberComparison/0, integerComparison/2, integerComparison/0, charComparison/2, charComparison/0, booleanComparison/2, booleanComparison/0]).
stringComparison(X, Y@1) ->
  (array:from_list([(X =:= Y@1),(X =/= Y@1),(X < Y@1),(X =< Y@1),(X >= Y@1),(X > Y@1)])).
stringComparison() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([(X =:= Y),(X =/= Y),(X < Y),(X =< Y),(X >= Y),(X > Y)]))
      end)
  end).
numberComparison(X, Y@1) ->
  (array:from_list([(X =:= Y@1),(X =/= Y@1),(X < Y@1),(X =< Y@1),(X >= Y@1),(X > Y@1)])).
numberComparison() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([(X =:= Y),(X =/= Y),(X < Y),(X =< Y),(X >= Y),(X > Y)]))
      end)
  end).
integerComparison(X, Y@1) ->
  (array:from_list([(X =:= Y@1),(X =/= Y@1),(X < Y@1),(X =< Y@1),(X >= Y@1),(X > Y@1)])).
integerComparison() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([(X =:= Y),(X =/= Y),(X < Y),(X =< Y),(X >= Y),(X > Y)]))
      end)
  end).
charComparison(X, Y@1) ->
  (array:from_list([(X =:= Y@1),(X =/= Y@1),(X < Y@1),(X =< Y@1),(X >= Y@1),(X > Y@1)])).
charComparison() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([(X =:= Y),(X =/= Y),(X < Y),(X =< Y),(X >= Y),(X > Y)]))
      end)
  end).
booleanComparison(X, Y@1) ->
  (array:from_list([(X =:= Y@1),(X =/= Y@1),(X < Y@1),(X =< Y@1),(X >= Y@1),(X > Y@1)])).
booleanComparison() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([(X =:= Y),(X =/= Y),(X < Y),(X =< Y),(X >= Y),(X > Y)]))
      end)
  end).

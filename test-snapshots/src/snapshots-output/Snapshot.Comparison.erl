-module(snapshot_comparison@ps).
-export([stringComparison/0, stringComparison/2, numberComparison/0, numberComparison/2, integerComparison/0, integerComparison/2, charComparison/0, charComparison/2, booleanComparison/0, booleanComparison/2]).
-compile(no_auto_import).
stringComparison() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (stringComparison(V@0, V@1))
      end)
  end).
stringComparison(X, Y) ->
  (array:from_list([(X =:= Y),(X =/= Y),(X < Y),(X =< Y),(X >= Y),(X > Y)])).
numberComparison() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (numberComparison(V@0, V@1))
      end)
  end).
numberComparison(X, Y) ->
  (array:from_list([(X =:= Y),(X =/= Y),(X < Y),(X =< Y),(X >= Y),(X > Y)])).
integerComparison() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (integerComparison(V@0, V@1))
      end)
  end).
integerComparison(X, Y) ->
  (array:from_list([(X =:= Y),(X =/= Y),(X < Y),(X =< Y),(X >= Y),(X > Y)])).
charComparison() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (charComparison(V@0, V@1))
      end)
  end).
charComparison(X, Y) ->
  (array:from_list([(X =:= Y),(X =/= Y),(X < Y),(X =< Y),(X >= Y),(X > Y)])).
booleanComparison() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (booleanComparison(V@0, V@1))
      end)
  end).
booleanComparison(X, Y) ->
  (array:from_list([(X =:= Y),(X =/= Y),(X < Y),(X =< Y),(X >= Y),(X > Y)])).

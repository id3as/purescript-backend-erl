-module(snapshot_comparison@ps).
-export([stringComparison/0, numberComparison/0, integerComparison/0, charComparison/0, booleanComparison/0]).
stringComparison() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([(X =:= Y),(X =/= Y),(X < Y),(X =< Y),(X >= Y),(X > Y)]))
      end)
  end).
numberComparison() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([(X =:= Y),(X =/= Y),(X < Y),(X =< Y),(X >= Y),(X > Y)]))
      end)
  end).
integerComparison() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([(X =:= Y),(X =/= Y),(X < Y),(X =< Y),(X >= Y),(X > Y)]))
      end)
  end).
charComparison() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([(X =:= Y),(X =/= Y),(X < Y),(X =< Y),(X >= Y),(X > Y)]))
      end)
  end).
booleanComparison() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([(X =:= Y),(X =/= Y),(X < Y),(X =< Y),(X >= Y),(X > Y)]))
      end)
  end).

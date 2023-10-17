-module(snapshot_comparison@ps).
-export([ stringComparison/0
        , stringComparison/2
        , numberComparison/0
        , numberComparison/2
        , integerComparison/0
        , integerComparison/2
        , charComparison/0
        , charComparison/2
        , booleanComparison/0
        , booleanComparison/2
        ]).
-compile(no_auto_import).
stringComparison() ->
  fun
    (X) ->
      fun
        (Y) ->
          stringComparison(X, Y)
      end
  end.

stringComparison(X, Y) ->
  array:from_list([X =:= Y, X =/= Y, X < Y, X =< Y, X >= Y, X > Y]).

numberComparison() ->
  fun
    (X) ->
      fun
        (Y) ->
          numberComparison(X, Y)
      end
  end.

numberComparison(X, Y) ->
  array:from_list([X =:= Y, X =/= Y, X < Y, X =< Y, X >= Y, X > Y]).

integerComparison() ->
  fun
    (X) ->
      fun
        (Y) ->
          integerComparison(X, Y)
      end
  end.

integerComparison(X, Y) ->
  array:from_list([X =:= Y, X =/= Y, X < Y, X =< Y, X >= Y, X > Y]).

charComparison() ->
  fun
    (X) ->
      fun
        (Y) ->
          charComparison(X, Y)
      end
  end.

charComparison(X, Y) ->
  array:from_list([X =:= Y, X =/= Y, X < Y, X =< Y, X >= Y, X > Y]).

booleanComparison() ->
  fun
    (X) ->
      fun
        (Y) ->
          booleanComparison(X, Y)
      end
  end.

booleanComparison(X, Y) ->
  array:from_list([X =:= Y, X =/= Y, X < Y, X =< Y, X >= Y, X > Y]).


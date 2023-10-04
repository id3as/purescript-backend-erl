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
    (X@Local) ->
      fun
        (Y@Local@1) ->
          stringComparison(X@Local, Y@Local@1)
      end
  end.

stringComparison(X, Y) ->
  array:from_list([X =:= Y, X =/= Y, X < Y, X =< Y, X >= Y, X > Y]).

numberComparison() ->
  fun
    (X@Local) ->
      fun
        (Y@Local@1) ->
          numberComparison(X@Local, Y@Local@1)
      end
  end.

numberComparison(X, Y) ->
  array:from_list([X =:= Y, X =/= Y, X < Y, X =< Y, X >= Y, X > Y]).

integerComparison() ->
  fun
    (X@Local) ->
      fun
        (Y@Local@1) ->
          integerComparison(X@Local, Y@Local@1)
      end
  end.

integerComparison(X, Y) ->
  array:from_list([X =:= Y, X =/= Y, X < Y, X =< Y, X >= Y, X > Y]).

charComparison() ->
  fun
    (X@Local) ->
      fun
        (Y@Local@1) ->
          charComparison(X@Local, Y@Local@1)
      end
  end.

charComparison(X, Y) ->
  array:from_list([X =:= Y, X =/= Y, X < Y, X =< Y, X >= Y, X > Y]).

booleanComparison() ->
  fun
    (X@Local) ->
      fun
        (Y@Local@1) ->
          booleanComparison(X@Local, Y@Local@1)
      end
  end.

booleanComparison(X, Y) ->
  array:from_list([X =:= Y, X =/= Y, X < Y, X =< Y, X >= Y, X > Y]).


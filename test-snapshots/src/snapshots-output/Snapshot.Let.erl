% Snapshot.Let
-module(snapshot_let@ps).
-export([ letRecursive/0
        , letRecursive/1
        , letChain/0
        , letChain/1
        , isOdd/0
        , isOdd/1
        , isEven/0
        , isEven/1
        ]).
-compile(no_auto_import).
letRecursive() ->
  fun letRecursive/1.

letRecursive(X) ->
  if
    X =:= 0 ->
      0;
    true ->
      letRecursive(X - 1)
  end.

letChain() ->
  fun letChain/1.

letChain(X) ->
  begin
    A = X + X,
    B = A + A,
    C = B + B,
    ((A + B) + C) + (C * C)
  end.

isOdd() ->
  fun isOdd/1.

isOdd(X) ->
  if
    X =:= 1 ->
      false;
    true ->
      isEven(X - 1)
  end.

isEven() ->
  fun isEven/1.

isEven(X) ->
  if
    X =:= 0 ->
      true;
    true ->
      begin
        V = X - 1,
        if
          V =:= 1 ->
            false;
          true ->
            isEven(V - 1)
        end
      end
  end.


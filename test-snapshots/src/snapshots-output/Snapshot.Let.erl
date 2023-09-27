-module(snapshot_let@ps).
-export([letRecursive/0, letRecursive/1, letChain/0, letChain/1, isOdd/0, isOdd/1, isEven/0, isEven/1]).
-compile(no_auto_import).
letRecursive() ->
  (fun
    (X@Local) ->
      (letRecursive(X@Local))
  end).
letRecursive(X) ->
  case (X =:= 0) of
    true ->
      0;
    _ ->
      ((snapshot_let@ps:letRecursive())((X - 1)))
  end.
letChain() ->
  (fun
    (X@Local) ->
      (letChain(X@Local))
  end).
letChain(X) ->
  begin
    A = (X + X),
    B = (A + A),
    C = (B + B),
    (((A + B) + C) + (C * C))
  end.
isOdd() ->
  (fun
    (X@Local) ->
      (isOdd(X@Local))
  end).
isOdd(X) ->
  case (X =:= 1) of
    true ->
      false;
    _ ->
      ((snapshot_let@ps:isEven())((X - 1)))
  end.
isEven() ->
  (fun
    (X@Local) ->
      (isEven(X@Local))
  end).
isEven(X) ->
  case (X =:= 0) of
    true ->
      true;
    _ ->
      begin
        V = (X - 1),
        case (V =:= 1) of
          true ->
            false;
          _ ->
            ((snapshot_let@ps:isEven())((V - 1)))
        end
      end
  end.

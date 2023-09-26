-module(snapshot_let@ps).
-export([letRecursive/0, letRecursive/1, letChain/0, letChain/1, isOdd/0, isOdd/1, isEven/0, isEven/1]).
letRecursive() ->
  (fun
    (V@0) ->
      (letRecursive(V@0))
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
    (V@0) ->
      (letChain(V@0))
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
    (V@0) ->
      (isOdd(V@0))
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
    (V@0) ->
      (isEven(V@0))
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

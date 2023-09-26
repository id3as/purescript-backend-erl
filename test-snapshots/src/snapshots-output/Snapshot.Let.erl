-module(snapshot_let@ps).
-export([letRecursive/1, letRecursive/0, letChain/1, letChain/0, isOdd/1, isOdd/0, isEven/1, isEven/0]).
letRecursive(X) ->
  case (X =:= 0) of
    true ->
      0;
    _ ->
      ((snapshot_let@ps:letRecursive())((X - 1)))
  end.
letRecursive() ->
  (fun
    (X) ->
      case (X =:= 0) of
        true ->
          0;
        _ ->
          ((snapshot_let@ps:letRecursive())((X - 1)))
      end
  end).
letChain(X) ->
  begin
    A = (X + X),
    B = (A + A),
    C = (B + B),
    (((A + B) + C) + (C * C))
  end.
letChain() ->
  (fun
    (X) ->
      begin
        A = (X + X),
        B = (A + A),
        C = (B + B),
        (((A + B) + C) + (C * C))
      end
  end).
isOdd(X) ->
  case (X =:= 1) of
    true ->
      false;
    _ ->
      ((snapshot_let@ps:isEven())((X - 1)))
  end.
isOdd() ->
  (fun
    (X) ->
      case (X =:= 1) of
        true ->
          false;
        _ ->
          ((snapshot_let@ps:isEven())((X - 1)))
      end
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
isEven() ->
  (fun
    (X) ->
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
      end
  end).

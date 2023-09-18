-module(snapshot_let@ps).
-export([letRecursive/0, letChain/0, isOdd/0, isEven/0]).
letRecursive() ->
  (fun
    (X@0) ->
      case (X@0 =:= 0) of
        true ->
          0;
        _ ->
          ((letRecursive())((X@0 - 1)))
      end
  end).
letChain() ->
  (fun
    (X@0) ->
      begin
        A@1 = (X@0 + X@0),
        B@2 = (A@1 + A@1),
        C@3 = (B@2 + B@2),
        (((A@1 + B@2) + C@3) + (C@3 * C@3))
      end
  end).
isOdd() ->
  (fun
    (X@0) ->
      case (X@0 =:= 1) of
        true ->
          false;
        _ ->
          ((isEven())((X@0 - 1)))
      end
  end).
isEven() ->
  (fun
    (X@0) ->
      case (X@0 =:= 0) of
        true ->
          true;
        _ ->
          begin
            V@1 = (X@0 - 1),
            case (V@1 =:= 1) of
              true ->
                false;
              _ ->
                ((isEven())((V@1 - 1)))
            end
          end
      end
  end).

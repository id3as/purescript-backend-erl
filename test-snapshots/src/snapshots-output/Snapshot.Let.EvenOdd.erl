-module(snapshot_let_evenOdd@ps).
-export([result/0]).
-compile(no_auto_import).
result() ->
  begin
    IsOdd@Mutual = (fun
      (IsOdd@Local, IsEven@Local) ->
        (fun
          (V) ->
            case (V =:= 0) of
              true ->
                false;
              _ ->
                ((IsEven@Local(IsOdd@Local, IsEven@Local))((V - 1)))
            end
        end)
    end),
    IsEven@Mutual = (fun
      (IsOdd@Local, IsEven@Local) ->
        (fun
          (V) ->
            case (V =:= 0) of
              true ->
                true;
              _ ->
                ((IsOdd@Local(IsOdd@Local, IsEven@Local))((V - 1)))
            end
        end)
    end),
    IsOdd = (IsOdd@Mutual(IsOdd@Mutual, IsEven@Mutual)),
    IsEven = (IsEven@Mutual(IsOdd@Mutual, IsEven@Mutual)),
    {tuple,(IsEven(5)),(IsOdd(5))}
  end.

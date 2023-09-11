-module(snapshot_let_evenOdd@ps).
-export([result/0]).
result() ->
  begin
    IsOdd@Mutual@0 = (fun
      (IsOdd@Local@0, IsEven@Local@0) ->
        (fun
          (V@1) ->
            case (V@1 =:= 0) of
              true ->
                false;
              _ ->
                ((IsEven@Local@0(IsOdd@Local@0, IsEven@Local@0))((V@1 - 1)))
            end
        end)
    end),
    IsEven@Mutual@0 = (fun
      (IsOdd@Local@0, IsEven@Local@0) ->
        (fun
          (V@1) ->
            case (V@1 =:= 0) of
              true ->
                true;
              _ ->
                ((IsOdd@Local@0(IsOdd@Local@0, IsEven@Local@0))((V@1 - 1)))
            end
        end)
    end),
    IsOdd@0 = (IsOdd@Mutual@0(IsOdd@Mutual@0, IsEven@Mutual@0)),
    IsEven@0 = (IsEven@Mutual@0(IsOdd@Mutual@0, IsEven@Mutual@0)),
    {tuple,(IsEven@0(5)),(IsOdd@0(5))}
  end.

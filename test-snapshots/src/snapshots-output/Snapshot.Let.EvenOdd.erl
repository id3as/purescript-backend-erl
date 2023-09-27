-module(snapshot_let_evenOdd@ps).
-export([result/0]).
-compile(no_auto_import).
result() ->
  begin
    IsOdd@MutualFn = (fun
      (IsOdd@LocalFn, IsEven@LocalFn) ->
        (fun
          (V) ->
            case (V =:= 0) of
              true ->
                false;
              _ ->
                ((IsEven@LocalFn(IsOdd@LocalFn, IsEven@LocalFn))((V - 1)))
            end
        end)
    end),
    IsEven@MutualFn = (fun
      (IsOdd@LocalFn, IsEven@LocalFn) ->
        (fun
          (V) ->
            case (V =:= 0) of
              true ->
                true;
              _ ->
                ((IsOdd@LocalFn(IsOdd@LocalFn, IsEven@LocalFn))((V - 1)))
            end
        end)
    end),
    IsOdd = (IsOdd@MutualFn(IsOdd@MutualFn, IsEven@MutualFn)),
    IsEven = (IsEven@MutualFn(IsOdd@MutualFn, IsEven@MutualFn)),
    {tuple,(IsEven(5)),(IsOdd(5))}
  end.

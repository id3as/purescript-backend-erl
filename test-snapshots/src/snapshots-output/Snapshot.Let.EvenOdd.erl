-module(snapshot_let_evenOdd@ps).
-export([result/0]).
-compile(no_auto_import).
result() ->
  begin
    IsOdd@MutualFn = fun
      (IsOdd@LocalFn, IsEven@LocalFn) ->
        fun
          (V) ->
            if
              V =:= 0 ->
                false;
              true ->
                (IsEven@LocalFn(IsOdd@LocalFn, IsEven@LocalFn))(V - 1)
            end
        end
    end,
    IsEven@MutualFn = fun
      (IsOdd@LocalFn, IsEven@LocalFn) ->
        fun
          (V) ->
            if
              V =:= 0 ->
                true;
              true ->
                (IsOdd@LocalFn(IsOdd@LocalFn, IsEven@LocalFn))(V - 1)
            end
        end
    end,
    IsOdd = IsOdd@MutualFn(IsOdd@MutualFn, IsEven@MutualFn),
    IsEven = IsEven@MutualFn(IsOdd@MutualFn, IsEven@MutualFn),
    {tuple, IsEven(5), IsOdd(5)}
  end.


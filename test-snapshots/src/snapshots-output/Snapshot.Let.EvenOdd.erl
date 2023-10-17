-module(snapshot_let_evenOdd@ps).
-export([result/0]).
-compile(no_auto_import).
result() ->
  begin
    IsOdd = fun
      (IsOdd, IsEven) ->
        fun
          (V) ->
            if
              V =:= 0 ->
                false;
              true ->
                (IsEven(IsOdd, IsEven))(V - 1)
            end
        end
    end,
    IsEven = fun
      (IsOdd@1, IsEven) ->
        fun
          (V) ->
            if
              V =:= 0 ->
                true;
              true ->
                (IsOdd@1(IsOdd@1, IsEven))(V - 1)
            end
        end
    end,
    IsOdd@1 = IsOdd(IsOdd, IsEven),
    IsEven@1 = IsEven(IsOdd, IsEven),
    {tuple, IsEven@1(5), IsOdd@1(5)}
  end.


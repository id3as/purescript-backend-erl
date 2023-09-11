-module(snapshot_let_fib@ps).
-export([result/0]).
result() ->
  begin
    Fib@0 = (fun
      Fib@0 (V@1) ->
        case (V@1 < 2) of
          true ->
            V@1;
          _ ->
            ((Fib@0((V@1 - 1))) + (Fib@0((V@1 - 2))))
        end
    end),
    (Fib@0(6))
  end.

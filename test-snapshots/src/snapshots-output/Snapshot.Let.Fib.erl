-module(snapshot_let_fib@ps).
-export([result/0]).
result() ->
  begin
    Fib = (fun
      Fib (V) ->
        case (V < 2) of
          true ->
            V;
          _ ->
            ((Fib((V - 1))) + (Fib((V - 2))))
        end
    end),
    (Fib(6))
  end.

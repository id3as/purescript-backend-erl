-module(snapshot_let_fib@ps).
-export([result/0]).
-compile(no_auto_import).
result() ->
  begin
    Fib = (fun
      Fib (V) ->
        if
          V < 2 ->
            V;
          true ->
            (Fib(V - 1)) + (Fib(V - 2))
        end
    end),
    Fib(6)
  end.


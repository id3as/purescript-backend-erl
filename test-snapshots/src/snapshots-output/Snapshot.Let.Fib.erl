-module(snapshot_let_fib@ps).
-export([result/0]).
-compile(no_auto_import).
result() ->
  begin
    Fib = (fun
      Fib (V) ->
        case (V < 2) of
          true ->
            V;
          _ ->
            (((fun
              (V@RecLocal) ->
                (Fib(V@RecLocal))
            end)((V - 1))) + ((fun
              (V@RecLocal) ->
                (Fib(V@RecLocal))
            end)((V - 2))))
        end
    end),
    ((fun
      (V@RecLocal) ->
        (Fib(V@RecLocal))
    end)(6))
  end.

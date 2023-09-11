-module(snapshot_let_fib@ps).
-export([result/0]).
result() ->
  begin
    Fib@Mutual@0 = (fun
      (Fib@Local@0) ->
        (fun
          (V@1) ->
            case (V@1 < 2) of
              true ->
                V@1;
              _ ->
                (((Fib@Local@0(Fib@Local@0))((V@1 - 1))) + ((Fib@Local@0(Fib@Local@0))((V@1 - 2))))
            end
        end)
    end),
    Fib@0 = (Fib@Mutual@0(Fib@Mutual@0)),
    (Fib@0(6))
  end.

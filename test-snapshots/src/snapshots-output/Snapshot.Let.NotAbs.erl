-module(snapshot_let_notAbs@ps).
-export([result/0]).
-compile(no_auto_import).
result() ->
  begin
    FibAnd = fun
      FibAnd () ->
        { tuple
        , <<"fib">>
        , fun
            (N) ->
              if
                N < 2 ->
                  N;
                true ->
                  ((erlang:element(3, FibAnd()))(N - 1))
                    + ((erlang:element(3, FibAnd()))(N - 2))
              end
          end
        }
    end,
    {tuple, erlang:element(2, FibAnd()), (erlang:element(3, FibAnd()))(6)}
  end.


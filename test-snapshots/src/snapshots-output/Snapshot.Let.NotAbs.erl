-module(snapshot_let_notAbs@ps).
-export([result/0]).
-compile(no_auto_import).
result() ->
  begin
    FibAnd@Rec = (fun
      FibAnd@Rec () ->
        { tuple
        , <<"fib">>
        , fun
          (N) ->
            if
              N < 2 ->
                N;
              true ->
                ((erlang:element(3, FibAnd@Rec()))(N - 1))
                  + ((erlang:element(3, FibAnd@Rec()))(N - 2))
            end
        end
        }
    end),
    { tuple
    , erlang:element(2, FibAnd@Rec())
    , (erlang:element(3, FibAnd@Rec()))(6)
    }
  end.


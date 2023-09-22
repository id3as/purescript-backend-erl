-module(snapshot_let_notAbs@ps).
-export([result/0]).
result() ->
  begin
    FibAnd@Rec = (fun
      FibAnd@Rec () ->
        {tuple,<<"fib">>,(fun
          (N) ->
            case (N < 2) of
              true ->
                N;
              _ ->
                (((erlang:element(3, (FibAnd@Rec())))((N - 1))) + ((erlang:element(3, (FibAnd@Rec())))((N - 2))))
            end
        end)}
    end),
    {tuple,(erlang:element(2, (FibAnd@Rec()))),((erlang:element(3, (FibAnd@Rec())))(6))}
  end.

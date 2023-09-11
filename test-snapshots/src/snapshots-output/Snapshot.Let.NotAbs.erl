-module(snapshot_let_notAbs@ps).
-export([result/0]).
result() ->
  begin
    FibAnd@Rec@0 = (fun
      FibAnd@Rec@0 () ->
        {tuple,<<"fib">>,(fun
          (N@1) ->
            case (N@1 < 2) of
              true ->
                N@1;
              _ ->
                (((erlang:element(3, (FibAnd@Rec@0())))((N@1 - 1))) + ((erlang:element(3, (FibAnd@Rec@0())))((N@1 - 2))))
            end
        end)}
    end),
    {tuple,(erlang:element(2, (FibAnd@Rec@0()))),((erlang:element(3, (FibAnd@Rec@0())))(6))}
  end.

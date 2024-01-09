-module(snapshot_let_notAbs@ps).
-export(['result.fibAnd'/0, result/0]).
-compile(no_auto_import).
'result.fibAnd'() ->
  { tuple
  , <<"fib">>
  , fun
      (N) ->
        if
          N < 2 ->
            N;
          true ->
            ((erlang:element(3, 'result.fibAnd'()))(N - 1))
              + ((erlang:element(3, 'result.fibAnd'()))(N - 2))
        end
    end
  }.

result() ->
  begin
    V = 'result.fibAnd'(),
    {tuple, erlang:element(2, V), (erlang:element(3, V))(6)}
  end.


% Snapshot.Let.NotAbs
-module(snapshot_let_notAbs@ps).
-export(['result.fibAnd'/0, result/0]).
-compile(no_auto_import).
-define( MEMOIZE_AS(Key, Expr)
       , case persistent_term:get(Key, undefined) of
           undefined ->
             begin
               MemoizeAsResult = Expr,
               persistent_term:put(Key, MemoizeAsResult),
               MemoizeAsResult
             end;
           MemoizeAsResult ->
             MemoizeAsResult
         end
       ).

'result.fibAnd'() ->
  ?MEMOIZE_AS(
    {snapshot_let_notAbs@ps, 'result.fibAnd', '(memoized)'},
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
    }
  ).

result() ->
  begin
    V = 'result.fibAnd'(),
    {tuple, erlang:element(2, V), (erlang:element(3, V))(6)}
  end.


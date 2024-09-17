% Snapshot.Let.Fib
-module(snapshot_let_fib@ps).
-export(['result.fib'/0, 'result.fib'/1, result/0]).
-compile(no_auto_import).
'result.fib'() ->
  fun 'result.fib'/1.

'result.fib'(V) ->
  if
    V < 2 ->
      V;
    true ->
      ('result.fib'(V - 1)) + ('result.fib'(V - 2))
  end.

result() ->
  'result.fib'(6).


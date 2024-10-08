% Snapshot.Let.EvenOdd
-module(snapshot_let_evenOdd@ps).
-export([ 'result.isOdd'/0
        , 'result.isOdd'/1
        , 'result.isEven'/0
        , 'result.isEven'/1
        , result/0
        ]).
-compile(no_auto_import).
'result.isOdd'() ->
  fun 'result.isOdd'/1.

'result.isOdd'(V) ->
  if
    V =:= 0 ->
      false;
    true ->
      'result.isEven'(V - 1)
  end.

'result.isEven'() ->
  fun 'result.isEven'/1.

'result.isEven'(V) ->
  if
    V =:= 0 ->
      true;
    true ->
      'result.isOdd'(V - 1)
  end.

result() ->
  {tuple, 'result.isEven'(5), 'result.isOdd'(5)}.


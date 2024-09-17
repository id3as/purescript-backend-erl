% Snapshot.UncurriedFunction
-module(snapshot_uncurriedFunction@ps).
-export([ test4a/0
        , test4a/1
        , test4b/0
        , test3a/0
        , test3a/2
        , test3b/0
        , test2a/0
        , test2a/2
        , test2b/0
        , test1a/0
        , test1b/0
        , 'main.0'/0
        , main/0
        ]).
-compile(no_auto_import).
test4a() ->
  fun
    (A) ->
      test4a(A)
  end.

test4a(A) ->
  begin
    V = effect_console@foreign:log(A),
    V(),
    A
  end.

test4b() ->
  fun
    () ->
      test4a(<<"test4b">>)
  end.

test3a() ->
  fun
    (V, B) ->
      test3a(V, B)
  end.

test3a(_, B) ->
  B.

test3b() ->
  test3a(1, 2).

test2a() ->
  fun
    (A, V) ->
      test2a(A, V)
  end.

test2a(A, _) ->
  A.

test2b() ->
  test2a(1, 2).

test1a() ->
  data_function_uncurried@foreign:mkFn0(fun
    (_) ->
      1
  end).

test1b() ->
  data_function_uncurried@foreign:runFn0(test1a()).

'main.0'() ->
  (test_assert@ps:assert())((test1b()) =:= 1).

main() ->
  fun
    () ->
      begin
        V = test_assert@ps:assert(),
        ('main.0'())(),
        (V((test2b()) =:= 1))(),
        (V((test3b()) =:= 2))(),
        V@1 = (test4b())(),
        (V(V@1 =:= <<"test4b">>))(),
        W = test4a(<<"test4b">>),
        (V(W =:= <<"test4b">>))()
      end
  end.


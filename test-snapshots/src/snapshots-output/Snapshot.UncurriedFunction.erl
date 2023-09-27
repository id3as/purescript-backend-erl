-module(snapshot_uncurriedFunction@ps).
-export([test4a/0, test4a/1, test4b/0, test3a/0, test3a/2, test3b/0, test2a/0, test2a/2, test2b/0, test1a/0, test1b/0, main/0]).
-compile(no_auto_import).
test4a() ->
  (fun
    (V@0) ->
      (test4a(V@0))
  end).
test4a(A) ->
  begin
    V = ((effect_console@ps:log())(A)),
    _ = (V()),
    A
  end.
test4b() ->
  (fun
    () ->
      ((snapshot_uncurriedFunction@ps:test4a())(<<"test4b">>))
  end).
test3a() ->
  (fun
    (V@0, V@1) ->
      (test3a(V@0, V@1))
  end).
test3a(_, B) ->
  B.
test3b() ->
  ((snapshot_uncurriedFunction@ps:test3a())(1, 2)).
test2a() ->
  (fun
    (V@0, V@1) ->
      (test2a(V@0, V@1))
  end).
test2a(A, _) ->
  A.
test2b() ->
  ((snapshot_uncurriedFunction@ps:test2a())(1, 2)).
test1a() ->
  ((data_function_uncurried@ps:mkFn0())((fun
    (_) ->
      1
  end))).
test1b() ->
  ((data_function_uncurried@ps:runFn0())((snapshot_uncurriedFunction@ps:test1a()))).
main() ->
  begin
    V = ((test_assert@ps:assert())(((snapshot_uncurriedFunction@ps:test1b()) =:= 1))),
    (fun
      () ->
        begin
          _ = (V()),
          _ = (((test_assert@ps:assert())(((snapshot_uncurriedFunction@ps:test2b()) =:= 1)))()),
          _ = (((test_assert@ps:assert())(((snapshot_uncurriedFunction@ps:test3b()) =:= 2)))()),
          V@1 = ((snapshot_uncurriedFunction@ps:test4b())()),
          _ = (((test_assert@ps:assert())((V@1 =:= <<"test4b">>)))()),
          W = ((snapshot_uncurriedFunction@ps:test4a())(<<"test4b">>)),
          (((test_assert@ps:assert())((W =:= <<"test4b">>)))())
        end
    end)
  end.

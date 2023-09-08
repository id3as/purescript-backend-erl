-module(snapshot_uncurriedFunction@ps).
-compile(export_all).
test4a() ->
  (fun
    (A@0) ->
      begin
        V@1 = ((effect_console@ps:log())(A@0)),
        _@dollar__unused@2 = (V@1()),
        A@0
      end
  end).
test4b() ->
  (fun
    () ->
      ((snapshot_uncurriedFunction@ps:test4a())(<<"test4b">>))
  end).
test3a() ->
  (fun
    (V@0, B@1) ->
      B@1
  end).
test3b() ->
  ((snapshot_uncurriedFunction@ps:test3a())(1, 2)).
test2a() ->
  (fun
    (A@0, V@1) ->
      A@0
  end).
test2b() ->
  ((snapshot_uncurriedFunction@ps:test2a())(1, 2)).
test1a() ->
  ((data_function_uncurried@ps:mkFn0())((fun
    (V@0) ->
      1
  end))).
test1b() ->
  ((data_function_uncurried@ps:runFn0())((snapshot_uncurriedFunction@ps:test1a()))).
main() ->
  begin
    V@0 = ((test_assert@ps:assert())(((snapshot_uncurriedFunction@ps:test1b()) =:= 1))),
    (fun
      () ->
        begin
          _@dollar__unused@1 = (V@0()),
          _@dollar__unused@2 = (((test_assert@ps:assert())(((snapshot_uncurriedFunction@ps:test2b()) =:= 1)))()),
          _@dollar__unused@3 = (((test_assert@ps:assert())(((snapshot_uncurriedFunction@ps:test3b()) =:= 2)))()),
          V@4 = ((snapshot_uncurriedFunction@ps:test4b())()),
          _@dollar__unused@5 = (((test_assert@ps:assert())((V@4 =:= <<"test4b">>)))()),
          W@6 = ((snapshot_uncurriedFunction@ps:test4a())(<<"test4b">>)),
          (((test_assert@ps:assert())((W@6 =:= <<"test4b">>)))())
        end
    end)
  end.

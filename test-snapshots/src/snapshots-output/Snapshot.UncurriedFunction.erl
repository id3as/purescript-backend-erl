-module(snapshot_uncurriedFunction).
test4a() -> 
  ?uncurried_effect_abs.
test4b() -> 
  ?uncurried_effect_app.
test3a() -> 
  (fun
    (V@0, B@1) -> 
      B@1
  end).
test3b() -> 
  ((snapshot_uncurriedFunction:test3a())(1, 2)).
test2a() -> 
  (fun
    (A@0, V@1) -> 
      A@0
  end).
test2b() -> 
  ((snapshot_uncurriedFunction:test2a())(1, 2)).
test1a() -> 
  ((data_function_uncurried:mkFn0())((fun
    (V@0) -> 
      1
  end))).
test1b() -> 
  ((data_function_uncurried:runFn0())((snapshot_uncurriedFunction:test1a()))).
main() -> 
  begin
    V@0 = ((test_assert:assert())(((snapshot_uncurriedFunction:test1b()) =:= 1))),
    ?effect_bind
  end.

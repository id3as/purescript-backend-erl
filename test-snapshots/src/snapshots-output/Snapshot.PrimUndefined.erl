-module(snapshot_primUndefined).
testCase() -> 
  (fun
    (DictRing@0) -> 
      (maps:get(add, ((maps:get(Semiring0, DictRing@0))(undefined))))
  end).
main() -> 
  ((test_assert:assert())((((((snapshot_primUndefined:testCase())((data_ring:ringInt())))(1))(1)) =:= 2))).

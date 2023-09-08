-module(snapshot_primUndefined@ps).
-compile(export_all).
testCase() ->
  (fun
    (DictRing@0) ->
      (maps:get(add, ((maps:get('Semiring0', DictRing@0))(undefined))))
  end).
main() ->
  ((test_assert@ps:assert())((((((snapshot_primUndefined@ps:testCase())((data_ring@ps:ringInt())))(1))(1)) =:= 2))).

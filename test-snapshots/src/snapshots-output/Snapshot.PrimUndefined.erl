-module(snapshot_primUndefined@ps).
-export([testCase/0, testCase/1, main/0]).
-compile(no_auto_import).
testCase() ->
  (fun
    (V@0) ->
      (testCase(V@0))
  end).
testCase(DictRing) ->
  (maps:get(add, ((maps:get('Semiring0', DictRing))(undefined)))).
main() ->
  ((test_assert@ps:assert())((((((snapshot_primUndefined@ps:testCase())((data_ring@ps:ringInt())))(1))(1)) =:= 2))).

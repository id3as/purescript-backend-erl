-module(snapshot_primUndefined@ps).
-export([testCase/1, testCase/0, main/0]).
testCase(DictRing) ->
  (maps:get(add, ((maps:get('Semiring0', DictRing))(undefined)))).
testCase() ->
  (fun
    (DictRing) ->
      (maps:get(add, ((maps:get('Semiring0', DictRing))(undefined))))
  end).
main() ->
  ((test_assert@ps:assert())((((((snapshot_primUndefined@ps:testCase())((data_ring@ps:ringInt())))(1))(1)) =:= 2))).

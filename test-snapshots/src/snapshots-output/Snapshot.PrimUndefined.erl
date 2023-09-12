-module(snapshot_primUndefined@ps).
-export([testCase/0, main/0]).
testCase() ->
  (fun
    (DictRing@0) ->
      (maps:get(add, ((maps:get('Semiring0', DictRing@0))(undefined))))
  end).
main() ->
  ((test_assert@ps:assert())((((((testCase())((data_ring@ps:ringInt())))(1))(1)) =:= 2))).

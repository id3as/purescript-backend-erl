-module(snapshot_primUndefined@ps).
-export([testCase/0, testCase/1, main/0]).
-compile(no_auto_import).
testCase() ->
  fun
    (DictRing@Local) ->
      testCase(DictRing@Local)
  end.

testCase(DictRing) ->
  erlang:map_get(add, (erlang:map_get('Semiring0', DictRing))(undefined)).

main() ->
  (test_assert@ps:assert())
  (((((snapshot_primUndefined@ps:testCase())(data_ring@ps:ringInt()))(1))(1))
    =:= 2).


% Snapshot.PrimUndefined
-module(snapshot_primUndefined@ps).
-export([testCase/0, testCase/1, main/0]).
-compile(no_auto_import).
testCase() ->
  fun testCase/1.

testCase(#{ 'Semiring0' := DictRing }) ->
  erlang:map_get(add, DictRing(undefined)).

main() ->
  (test_assert@ps:assert())((((testCase(data_ring@ps:ringInt()))(1))(1)) =:= 2).


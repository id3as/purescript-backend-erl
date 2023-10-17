-module(snapshot_primUndefined@ps).
-export([testCase/0, testCase/1, main/0]).
-compile(no_auto_import).
testCase() ->
  fun
    (DictRing) ->
      testCase(DictRing)
  end.

testCase(#{ 'Semiring0' := DictRing@1 }) ->
  erlang:map_get(add, DictRing@1(undefined)).

main() ->
  (test_assert@ps:assert())((((testCase(data_ring@ps:ringInt()))(1))(1)) =:= 2).


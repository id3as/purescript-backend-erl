% Snapshot.PrimUndefined
-module(snapshot_primUndefined@ps).
-export([testCase/0, testCase/1, main/0]).
-compile(no_auto_import).
-define( MEMOIZE_AS(Key, _Metadata, Expr)
       , case persistent_term:get(Key, undefined) of
           undefined ->
             begin
               MemoizeAsResult = Expr,
               persistent_term:put(Key, MemoizeAsResult),
               MemoizeAsResult
             end;
           MemoizeAsResult ->
             MemoizeAsResult
         end
       ).

testCase() ->
  fun testCase/1.

testCase(#{ 'Semiring0' := DictRing }) ->
  erlang:map_get(add, DictRing(undefined)).

main() ->
  ?MEMOIZE_AS(
    {snapshot_primUndefined@ps, main, '(memoized)'},
    18,
    (test_assert@ps:assert())
    ((((testCase(data_ring@ps:ringInt()))(1))(1)) =:= 2)
  ).


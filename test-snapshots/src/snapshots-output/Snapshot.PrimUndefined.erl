-module('Snapshot.PrimUndefined').
-compile(export_all).
testCase() -> 
  (fun
    (DictRing@0) ->
      (maps:get(add, ((maps:get('Semiring0', DictRing@0))(undefined))))
  end).
main() -> 
  (('Test.Assert':assert())(((((('Snapshot.PrimUndefined':testCase())(('Data.Ring':ringInt())))(1))(1)) =:= 2))).

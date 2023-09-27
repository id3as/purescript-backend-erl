-module(snapshot_arrayIndex@ps).
-export([testAccessorGetIndex/0, testAccessorGetIndex/1, result/0]).
-compile(no_auto_import).
testAccessorGetIndex() ->
  (fun
    (V@Local) ->
      (testAccessorGetIndex(V@Local))
  end).
testAccessorGetIndex(V) ->
  case ((array:size(V)) =:= 1) of
    true ->
      (array:get(0, V));
    _ ->
      0
  end.
result() ->
  5.

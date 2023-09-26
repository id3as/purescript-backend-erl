-module(snapshot_arrayIndex@ps).
-export([testAccessorGetIndex/1, testAccessorGetIndex/0, result/0]).
testAccessorGetIndex(V) ->
  case ((array:size(V)) =:= 1) of
    true ->
      (array:get(0, V));
    _ ->
      0
  end.
testAccessorGetIndex() ->
  (fun
    (V) ->
      case ((array:size(V)) =:= 1) of
        true ->
          (array:get(0, V));
        _ ->
          0
      end
  end).
result() ->
  5.

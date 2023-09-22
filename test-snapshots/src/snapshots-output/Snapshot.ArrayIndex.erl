-module(snapshot_arrayIndex@ps).
-export([testAccessorGetIndex/0, result/0]).
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

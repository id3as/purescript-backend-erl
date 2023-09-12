-module(snapshot_arrayIndex@ps).
-export([testAccessorGetIndex/0, result/0]).
testAccessorGetIndex() ->
  (fun
    (V@0) ->
      case ((array:size(V@0)) =:= 1) of
        true ->
          (array:get(0, V@0));
        _ ->
          0
      end
  end).
result() ->
  begin
    V@0 = (array:from_list([5])),
    case ((array:size(V@0)) =:= 1) of
      true ->
        (array:get(0, V@0));
      _ ->
        0
    end
  end.

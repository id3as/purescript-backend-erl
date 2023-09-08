-module(snapshot_arrayIndex@ps).
-compile(export_all).
testAccessorGetIndex() ->
  (fun
    (V@0) ->
      case ((array:length(V@0)) =:= 1) of
        true ->
          (array:get(0, V@0));
        _ ->
          0
      end
  end).

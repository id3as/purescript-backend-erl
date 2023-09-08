-module(snapshot_literals_record@ps).
-compile(export_all).
recordAccess() ->
  (fun
    (V@0) ->
      (maps:get(fooBarBaz, V@0))
  end).

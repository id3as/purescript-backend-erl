-module('Snapshot.Literals.Record').
-compile(export_all).
recordAccess() -> 
  (fun
    (V@0) ->
      (maps:get(fooBarBaz, V@0))
  end).

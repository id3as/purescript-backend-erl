-module(snapshot_literals_record).
recordAccess() -> 
  (fun
    (V@0) -> 
      (maps:get(fooBarBaz, V@0))
  end).

-module(snapshot_literals_record@ps).
-export([recordAccess/0, recordAccess/1]).
recordAccess() ->
  (fun
    (V@0) ->
      (recordAccess(V@0))
  end).
recordAccess(V) ->
  (maps:get(fooBarBaz, V)).

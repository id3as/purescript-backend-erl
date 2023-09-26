-module(snapshot_literals_record@ps).
-export([recordAccess/1, recordAccess/0]).
recordAccess(V) ->
  (maps:get(fooBarBaz, V)).
recordAccess() ->
  (fun
    (V) ->
      (maps:get(fooBarBaz, V))
  end).

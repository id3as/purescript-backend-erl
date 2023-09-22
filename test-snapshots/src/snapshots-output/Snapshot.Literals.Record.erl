-module(snapshot_literals_record@ps).
-export([recordAccess/0]).
recordAccess() ->
  (fun
    (V) ->
      (maps:get(fooBarBaz, V))
  end).

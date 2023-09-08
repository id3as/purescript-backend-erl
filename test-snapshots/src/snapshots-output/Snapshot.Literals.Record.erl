-module(snapshot_literals_record@ps).
-export([recordAccess/0]).
recordAccess() ->
  (fun
    (V@0) ->
      (maps:get(fooBarBaz, V@0))
  end).

-module(snapshot_literals_record@ps).
-export([recordAccess/0, recordAccess/1]).
-compile(no_auto_import).
recordAccess() ->
  fun
    (V@Local) ->
      recordAccess(V@Local)
  end.

recordAccess(V) ->
  erlang:map_get(fooBarBaz, V).


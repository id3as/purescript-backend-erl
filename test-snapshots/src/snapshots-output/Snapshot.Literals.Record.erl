-module(snapshot_literals_record@ps).
-export([recordAccess/0, recordAccess/1]).
-compile(no_auto_import).
recordAccess() ->
  fun
    (V) ->
      recordAccess(V)
  end.

recordAccess(#{ fooBarBaz := V }) ->
  V.


% Snapshot.Literals.Record
-module(snapshot_literals_record@ps).
-export([recordAccess/0, recordAccess/1]).
-compile(no_auto_import).
recordAccess() ->
  fun recordAccess/1.

recordAccess(#{ fooBarBaz := V }) ->
  V.


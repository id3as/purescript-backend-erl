% Snapshot.Import
-module(snapshot_import@ps).
-export([fortyThree/0, result/0]).
-compile(no_auto_import).
fortyThree() ->
  snapshot_import_impl@foreign:addImpl(21, 22).

result() ->
  fortyThree().


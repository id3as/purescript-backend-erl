% Snapshot.Import.Impl
-module(snapshot_import_impl@ps).
-export([fortyTwo/0, result/0, addImpl/0]).
-compile(no_auto_import).
fortyTwo() ->
  snapshot_import_impl@foreign:addImpl(21, 21).

result() ->
  fortyTwo().

addImpl() ->
  fun
    (V) ->
      fun
        (V@1) ->
          snapshot_import_impl@foreign:addImpl(V, V@1)
      end
  end.


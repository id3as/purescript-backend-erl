-module(snapshot_import_impl@ps).
-export([fortyTwo/0, result/0, addImpl/0]).
fortyTwo() ->
  (((addImpl())(21))(21)).
result() ->
  (fortyTwo()).
addImpl() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (snapshot_import_impl@foreign:addImpl(V@0, V@1))
      end)
  end).

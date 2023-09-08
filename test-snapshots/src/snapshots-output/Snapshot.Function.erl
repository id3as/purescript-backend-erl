-module(snapshot_function).
-compile(export_all).
f() -> 
  (fun
    (X@0) ->
      (fun
        (Y@1) ->
          (array:from_list([X@0,Y@1,X@0,Y@1,X@0]))
      end)
  end).
g() -> 
  (fun
    (X@0) ->
      (fun
        (Y@1) ->
          (((snapshot_function:f())(X@0))(Y@1))
      end)
  end).

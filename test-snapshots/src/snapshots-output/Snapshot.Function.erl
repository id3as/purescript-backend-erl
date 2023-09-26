-module(snapshot_function@ps).
-export([f/2, f/0, g/2, g/0]).
f(X, Y@1) ->
  (array:from_list([X,Y@1,X,Y@1,X])).
f() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([X,Y,X,Y,X]))
      end)
  end).
g(X, Y@1) ->
  (((snapshot_function@ps:f())(X))(Y@1)).
g() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (((snapshot_function@ps:f())(X))(Y))
      end)
  end).

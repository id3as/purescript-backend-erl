-module(snapshot_function@ps).
-export([f/0, f/2, g/0, g/2]).
f() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (f(V@0, V@1))
      end)
  end).
f(X, Y) ->
  (array:from_list([X,Y,X,Y,X])).
g() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (g(V@0, V@1))
      end)
  end).
g(X, Y) ->
  (((snapshot_function@ps:f())(X))(Y)).

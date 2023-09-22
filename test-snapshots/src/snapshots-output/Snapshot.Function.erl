-module(snapshot_function@ps).
-export([f/0, g/0]).
f() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (array:from_list([X,Y,X,Y,X]))
      end)
  end).
g() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (((snapshot_function@ps:f())(X))(Y))
      end)
  end).

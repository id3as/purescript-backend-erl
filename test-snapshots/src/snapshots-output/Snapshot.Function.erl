-module(snapshot_function@ps).
-export([f/0, f/2, g/0, g/2]).
-compile(no_auto_import).
f() ->
  fun
    (X@Local) ->
      fun
        (Y@Local@1) ->
          f(X@Local, Y@Local@1)
      end
  end.

f(X, Y) ->
  array:from_list([X, Y, X, Y, X]).

g() ->
  fun
    (X@Local) ->
      fun
        (Y@Local@1) ->
          g(X@Local, Y@Local@1)
      end
  end.

g(X, Y) ->
  ((snapshot_function@ps:f())(X))(Y).


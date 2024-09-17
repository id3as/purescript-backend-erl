% Snapshot.Function
-module(snapshot_function@ps).
-export([f/0, f/2, g/0, g/2]).
-compile(no_auto_import).
f() ->
  fun
    (X) ->
      fun
        (Y) ->
          f(X, Y)
      end
  end.

f(X, Y) ->
  array:from_list([X, Y, X, Y, X]).

g() ->
  fun
    (X) ->
      fun
        (Y) ->
          g(X, Y)
      end
  end.

g(X, Y) ->
  f(X, Y).


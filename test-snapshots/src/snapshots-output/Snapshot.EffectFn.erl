-module(snapshot_effectFn@ps).
-export([x/0, x/1, y/0, y/2, main/0]).
-compile(no_auto_import).
x() ->
  fun
    (A) ->
      x(A)
  end.

x(A) ->
  fun
    (_, _) ->
      (effect_console@foreign:log(A))()
  end.

y() ->
  fun
    (A, B) ->
      y(A, B)
  end.

y(A, B) ->
  (x(<<"Hi">>))(A, B).

main() ->
  fun
    () ->
      y(unit, unit)
  end.


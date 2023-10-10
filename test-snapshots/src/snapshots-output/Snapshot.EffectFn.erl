-module(snapshot_effectFn@ps).
-export([x/0, x/1, y/0, y/2, main/0]).
-compile(no_auto_import).
x() ->
  fun
    (A@Local) ->
      x(A@Local)
  end.

x(A) ->
  fun
    (_, _) ->
      (effect_console@foreign:log(A))()
  end.

y() ->
  fun
    (A@Local, B@Local@1) ->
      y(A@Local, B@Local@1)
  end.

y(A, B) ->
  (x(<<"Hi">>))(A, B).

main() ->
  fun
    () ->
      y(unit, unit)
  end.


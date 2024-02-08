-module(test).
-compile(export_all).

test(X) ->
  begin

    a:b(erlang:map_get(2, X)),
    a:c(erlang:map_get(2, X)),
    a:d(erlang:map_get(2, X)),
    a:e(erlang:map_get(2, X))
  end.

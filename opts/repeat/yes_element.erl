-module(test).
-compile(export_all).

test(X) ->
  begin

    a:b(erlang:element(2, X)),
    % optimized from BIF to BEAM instruction
    % but still not memoized
    a:c(erlang:element(2, X)),
    a:d(erlang:element(2, X)),
    a:e(erlang:element(2, X))
  end.

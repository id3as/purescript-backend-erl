-module(test).
-compile(export_all).

test(X) ->
  begin
    V = erlang:element(2, X),
    a:b(V),
    a:c(V),
    a:d(V),
    a:e(V)
  end.

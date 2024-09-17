% Snapshot.Access
-module(snapshot_access@ps).
-export([foo/0, foo/1, f/0, f/1, bar/0, bar/1, g/0, g/1, h/0, h/1]).
-compile(no_auto_import).
foo() ->
  fun foo/1.

foo(I) ->
  #{ x => I + 9, y => I - 5 }.

f() ->
  fun f/1.

f(I) ->
  begin
    #{ x := V, y := V@1 } = foo(I),
    V * V@1
  end.

bar() ->
  fun bar/1.

bar(#{ x := V, y := V@1 }) ->
  (V - V@1) * V@1.

g() ->
  fun g/1.

g(I) ->
  begin
    V = #{ x := V@1, y := V@2 } = foo(I),
    (V@1 * V@2) + (bar(V))
  end.

h() ->
  fun h/1.

h(Mi) ->
  case Mi of
    {just, Mi@1} ->
      ((erlang:map_get(x, foo(Mi@1))) * (erlang:map_get(y, foo(Mi@1))))
        + (bar(foo(Mi@1)));
    _ ->
      0
  end.


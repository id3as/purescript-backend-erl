-module(snapshot_access@ps).
-export([foo/0, foo/1, f/0, f/1, bar/0, bar/1, g/0, g/1, h/0, h/1]).
-compile(no_auto_import).
-define( IS_KNOWN_TAG(Tag, Arity, V)
       , ((erlang:is_tuple(V))
         andalso ((1 =< (erlang:tuple_size(V)))
           andalso (Tag =:= (erlang:element(1, V)))))
       ).
foo() ->
  fun
    (I) ->
      foo(I)
  end.

foo(I) ->
  #{ x => I + 9, y => I - 5 }.

f() ->
  fun
    (I) ->
      f(I)
  end.

f(I) ->
  begin
    #{ x := V@1, y := V@2 } = foo(I),
    V@1 * V@2
  end.

bar() ->
  fun
    (V) ->
      bar(V)
  end.

bar(#{ x := V@1, y := V@2 }) ->
  (V@1 - V@2) * V@2.

g() ->
  fun
    (I) ->
      g(I)
  end.

g(I) ->
  begin
    V = #{ x := V@1, y := V@2 } = foo(I),
    (V@1 * V@2) + (bar(V))
  end.

h() ->
  fun
    (Mi) ->
      h(Mi)
  end.

h(Mi) ->
  if
    ?IS_KNOWN_TAG(just, 1, Mi) ->
      ((erlang:map_get(x, foo(erlang:element(2, Mi))))
        * (erlang:map_get(y, foo(erlang:element(2, Mi)))))
        + (bar(foo(erlang:element(2, Mi))));
    true ->
      0
  end.


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
    (I@Local) ->
      foo(I@Local)
  end.

foo(I) ->
  #{ x => I + 9, y => I - 5 }.

f() ->
  fun
    (I@Local) ->
      f(I@Local)
  end.

f(I) ->
  begin
    V = #{ x := V@@0, y := V@@1 } = foo(I),
    V@@0 * V@@1
  end.

bar() ->
  fun
    (V@Local) ->
      bar(V@Local)
  end.

bar(V) ->
  ((erlang:map_get(x, V)) - (erlang:map_get(y, V))) * (erlang:map_get(y, V)).

g() ->
  fun
    (I@Local) ->
      g(I@Local)
  end.

g(I) ->
  begin
    V = #{ x := V@@0, y := V@@1 } = foo(I),
    (V@@0 * V@@1) + (bar(V))
  end.

h() ->
  fun
    (Mi@Local) ->
      h(Mi@Local)
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


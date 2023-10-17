-module(snapshot_branch@ps).
-export([ i/0
        , i/2
        , h/0
        , h/1
        , g/0
        , g/1
        , f/0
        , f/3
        , result/0
        , dontInlineMe/0
        , dontInlineMe/1
        ]).
-compile(no_auto_import).
i() ->
  fun
    (V) ->
      fun
        (V1) ->
          i(V, V1)
      end
  end.

i(V, V1) ->
  if
    V ->
      not V1;
    not V1 ->
      true;
    V1 ->
      false;
    true ->
      erlang:throw({fail, <<"Failed pattern match">>})
  end.

h() ->
  fun
    (V) ->
      h(V)
  end.

h(V) ->
  if
    V =:= 3.14 ->
      3.14159;
    true ->
      erlang:throw({fail, <<"Failed pattern match">>})
  end.

g() ->
  fun
    (V) ->
      g(V)
  end.

g(V) ->
  if
    V =:= 0 ->
      1;
    V =:= 1 ->
      2;
    V =:= 2 ->
      3;
    true ->
      0
  end.

f() ->
  fun
    (X) ->
      fun
        (Y) ->
          fun
            (Z) ->
              f(X, Y, Z)
          end
      end
  end.

f(X, Y, Z) ->
  if
    X ->
      if
        Y ->
          if
            Z ->
              0;
            true ->
              1
          end;
        true ->
          2
      end;
    true ->
      3
  end.

result() ->
  #{ f0 => 0
   , f1 => 1
   , f2 => 2
   , f3 => 3
   , g0 => 0
   , g1 => 1
   , g2 => 2
   , g3 => 3
   , ittf => false
   , ifft => true
   , iftf => false
   , itft => true
   , h => 3.14159
   }.

dontInlineMe() ->
  fun
    (A) ->
      dontInlineMe(A)
  end.

dontInlineMe(A) ->
  A.


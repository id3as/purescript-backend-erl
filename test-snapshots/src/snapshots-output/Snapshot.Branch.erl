-module(snapshot_branch@ps).
-export([i/0, i/2, h/0, h/1, g/0, g/1, f/0, f/3, result/0, dontInlineMe/0, dontInlineMe/1]).
-compile(no_auto_import).
i() ->
  (fun
    (V@Local) ->
      (fun
        (V1@Local@1) ->
          (i(V@Local, V1@Local@1))
      end)
  end).
i(V, V1) ->
  case V of
    true ->
      (not V1);
    _ ->
      case (not V1) of
        true ->
          true;
        _ ->
          case V1 of
            true ->
              false;
            _ ->
              (erlang:throw({fail, <<"Failed pattern match">>}))
          end
      end
  end.
h() ->
  (fun
    (V@Local) ->
      (h(V@Local))
  end).
h(V) ->
  case (V =:= 3.14) of
    true ->
      3.14159;
    _ ->
      (erlang:throw({fail, <<"Failed pattern match">>}))
  end.
g() ->
  (fun
    (V@Local) ->
      (g(V@Local))
  end).
g(V) ->
  case (V =:= 0) of
    true ->
      1;
    _ ->
      case (V =:= 1) of
        true ->
          2;
        _ ->
          case (V =:= 2) of
            true ->
              3;
            _ ->
              0
          end
      end
  end.
f() ->
  (fun
    (X@Local) ->
      (fun
        (Y@Local@1) ->
          (fun
            (Z@Local@2) ->
              (f(X@Local, Y@Local@1, Z@Local@2))
          end)
      end)
  end).
f(X, Y, Z) ->
  case X of
    true ->
      case Y of
        true ->
          case Z of
            true ->
              0;
            _ ->
              1
          end;
        _ ->
          2
      end;
    _ ->
      3
  end.
result() ->
  #{f0 => 0,
  f1 => 1,
  f2 => 2,
  f3 => 3,
  g0 => 0,
  g1 => 1,
  g2 => 2,
  g3 => 3,
  ittf => false,
  ifft => true,
  iftf => false,
  itft => true,
  h => 3.14159}.
dontInlineMe() ->
  (fun
    (A@Local) ->
      (dontInlineMe(A@Local))
  end).
dontInlineMe(A) ->
  A.

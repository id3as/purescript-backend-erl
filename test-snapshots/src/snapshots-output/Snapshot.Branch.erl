-module(snapshot_branch@ps).
-export([i/0, h/0, g/0, f/0, result/0, dontInlineMe/0]).
i() ->
  (fun
    (V) ->
      (fun
        (V1) ->
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
                      (erlang:throw({fail,<<"Failed pattern match">>}))
                  end
              end
          end
      end)
  end).
h() ->
  (fun
    (V) ->
      case (V =:= 3.14) of
        true ->
          3.14159;
        _ ->
          (erlang:throw({fail,<<"Failed pattern match">>}))
      end
  end).
g() ->
  (fun
    (V) ->
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
      end
  end).
f() ->
  (fun
    (X) ->
      (fun
        (Y) ->
          (fun
            (Z) ->
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
              end
          end)
      end)
  end).
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
    (A) ->
      A
  end).

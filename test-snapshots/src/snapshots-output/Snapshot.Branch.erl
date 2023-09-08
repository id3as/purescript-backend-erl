-module(snapshot_branch).
-compile(export_all).
i() -> 
  (fun
    (V@0) ->
      (fun
        (V1@1) ->
          case V@0 of
            true ->
              (not V1@1);
            _ ->
              case (not V1@1) of
                true ->
                  true;
                _ ->
                  case V1@1 of
                    true ->
                      false;
                    _ ->
                      (erlang:throw({fail,<<"Failed pattern match"/utf8>>}))
                  end
              end
          end
      end)
  end).
g() -> 
  (fun
    (V@0) ->
      case (V@0 =:= 0) of
        true ->
          1;
        _ ->
          case (V@0 =:= 1) of
            true ->
              2;
            _ ->
              case (V@0 =:= 2) of
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
    (X@0) ->
      (fun
        (Y@1) ->
          (fun
            (Z@2) ->
              case X@0 of
                true ->
                  case Y@1 of
                    true ->
                      case Z@2 of
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

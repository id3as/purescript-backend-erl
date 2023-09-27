-module(snapshot_let_goArity@ps).
-export([foldr/0, foldr/2, result/0]).
-compile(no_auto_import).
foldr() ->
  (fun
    (F@Local) ->
      (fun
        (B@Local@1) ->
          (foldr(F@Local, B@Local@1))
      end)
  end).
foldr(F, B) ->
  begin
    Go = (fun
      Go (B@1, V) ->
        case ((erlang:is_tuple(V)) andalso ((1 =< (erlang:tuple_size(V))) andalso ((nil =:= (erlang:element(1, V))) andalso true))) of
          true ->
            B@1;
          _ ->
            case ((erlang:is_tuple(V)) andalso ((1 =< (erlang:tuple_size(V))) andalso ((cons =:= (erlang:element(1, V))) andalso true))) of
              true ->
                (((fun
                  (B@Local) ->
                    (fun
                      (V@Local@1) ->
                        (Go(B@Local, V@Local@1))
                    end)
                end)(((F((erlang:element(2, V))))(B@1))))((erlang:element(3, V))));
              _ ->
                (erlang:throw({fail,<<"Failed pattern match">>}))
            end
        end
    end),
    V = ((fun
      (B@Local) ->
        (fun
          (V@Local@1) ->
            (Go(B@Local, V@Local@1))
        end)
    end)(B)),
    Go@1 = (fun
      Go@1 (V@1, V1) ->
        case ((erlang:is_tuple(V1)) andalso ((1 =< (erlang:tuple_size(V1))) andalso ((nil =:= (erlang:element(1, V1))) andalso true))) of
          true ->
            V@1;
          _ ->
            case ((erlang:is_tuple(V1)) andalso ((1 =< (erlang:tuple_size(V1))) andalso ((cons =:= (erlang:element(1, V1))) andalso true))) of
              true ->
                (((fun
                  (V@Local) ->
                    (fun
                      (V1@Local@1) ->
                        (Go@1(V@Local, V1@Local@1))
                    end)
                end)({cons,(erlang:element(2, V1)),V@1}))((erlang:element(3, V1))));
              _ ->
                (erlang:throw({fail,<<"Failed pattern match">>}))
            end
        end
    end),
    V@1 = ((fun
      (V@Local) ->
        (fun
          (V1@Local@1) ->
            (Go@1(V@Local, V1@Local@1))
        end)
    end)({nil})),
    (fun
      (X) ->
        (V((V@1(X))))
    end)
  end.
result() ->
  ((((snapshot_let_goArity@ps:foldr())((data_semiring@ps:intAdd())))(1))({cons,2,{cons,3,{cons,4,{nil}}}})).

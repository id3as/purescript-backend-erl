-module(snapshot_fnXLazyBug@ps).
-export([zipWith4/0, zipWith4/5, result/0]).
-compile(no_auto_import).
-define( IS_KNOWN_TAG(Tag, Arity, V)
       , ((erlang:is_tuple(V))
         andalso (((Arity + 1) =:= (erlang:tuple_size(V)))
           andalso (Tag =:= (erlang:element(1, V)))))
       ).
zipWith4() ->
  fun
    (F) ->
      fun
        (As) ->
          fun
            (Bs) ->
              fun
                (Cs) ->
                  fun
                    (Ds) ->
                      zipWith4(F, As, Bs, Cs, Ds)
                  end
              end
          end
      end
  end.

zipWith4(F, As, Bs, Cs, Ds) ->
  begin
    Go =
      fun
        Go () ->
          fun
            (Acc, As1, Bs1, Cs1, Ds1) ->
              begin
                V = (erl_data_list_types@ps:uncons())(Ds1),
                V1 = (erl_data_list_types@ps:uncons())(Cs1),
                V2 = (erl_data_list_types@ps:uncons())(Bs1),
                V3 = (erl_data_list_types@ps:uncons())(As1),
                if
                  ?IS_KNOWN_TAG(just, 1, V3)
                    andalso (?IS_KNOWN_TAG(just, 1, V2)
                      andalso (?IS_KNOWN_TAG(just, 1, V1)
                        andalso ?IS_KNOWN_TAG(just, 1, V))) ->
                    begin
                      {just, #{ head := V@2, tail := V@3 }} = V,
                      {just, #{ head := V3@2, tail := V3@3 }} = V3,
                      {just, #{ head := V2@2, tail := V2@3 }} = V2,
                      {just, #{ head := V1@2, tail := V1@3 }} = V1,
                      (Go())
                      (
                        [ (((F(V3@2))(V2@2))(V1@2))(V@2) | Acc ],
                        V3@3,
                        V2@3,
                        V1@3,
                        V@3
                      )
                    end;
                  true ->
                    erl_data_list@foreign:reverse(Acc)
                end
              end
          end
      end,
    (Go())([], As, Bs, Cs, Ds)
  end.

result() ->
  zipWith4(
    fun
      (A) ->
        fun
          (B) ->
            fun
              (C) ->
                fun
                  (D) ->
                    (A * B) + (C * D)
                end
            end
        end
    end,
    [1, 2, 9],
    [0, 1],
    [3, 4],
    [5, 6, 7, 8]
  ).


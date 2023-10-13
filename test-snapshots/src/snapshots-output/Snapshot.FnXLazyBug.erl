-module(snapshot_fnXLazyBug@ps).
-export([zipWith4/0, zipWith4/5, result/0]).
-compile(no_auto_import).
-define( IS_KNOWN_TAG(Tag, Arity, V)
       , ((erlang:is_tuple(V))
         andalso ((1 =< (erlang:tuple_size(V)))
           andalso (Tag =:= (erlang:element(1, V)))))
       ).
zipWith4() ->
  fun
    (F@Local) ->
      fun
        (As@Local@1) ->
          fun
            (Bs@Local@2) ->
              fun
                (Cs@Local@3) ->
                  fun
                    (Ds@Local@4) ->
                      zipWith4(
                        F@Local,
                        As@Local@1,
                        Bs@Local@2,
                        Cs@Local@3,
                        Ds@Local@4
                      )
                  end
              end
          end
      end
  end.

zipWith4(F, As, Bs, Cs, Ds) ->
  begin
    Go@Rec = fun
      Go@Rec () ->
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
                    {_, V@@0 = #{ head := V@@0@@0, tail := V@@0@@1 }} = V,
                    {_, V1@@0 = #{ head := V1@@0@@0, tail := V1@@0@@1 }} = V1,
                    {_, V2@@0 = #{ head := V2@@0@@0, tail := V2@@0@@1 }} = V2,
                    {_, V3@@0 = #{ head := V3@@0@@0, tail := V3@@0@@1 }} = V3,
                    (Go@Rec())
                    (
                      [ (((F(V3@@0@@0))(V2@@0@@0))(V1@@0@@0))(V@@0@@0) | Acc ],
                      V3@@0@@1,
                      V2@@0@@1,
                      V1@@0@@1,
                      V@@0@@1
                    )
                  end;
                true ->
                  erl_data_list@foreign:reverse(Acc)
              end
            end
        end
    end,
    (Go@Rec())([], As, Bs, Cs, Ds)
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


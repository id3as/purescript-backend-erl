-module(snapshot_fnXLazyBug@ps).
-export([zipWith4/0, zipWith4/5, result/0]).
-compile(no_auto_import).
zipWith4() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (fun
            (V@2) ->
              (fun
                (V@3) ->
                  (fun
                    (V@4) ->
                      (zipWith4(V@0, V@1, V@2, V@3, V@4))
                  end)
              end)
          end)
      end)
  end).
zipWith4(F, As, Bs, Cs, Ds) ->
  begin
    Go@Rec = (fun
      Go@Rec () ->
        (fun
          (Acc, As1, Bs1, Cs1, Ds1) ->
            begin
              V = ((erl_data_list_types@ps:uncons())(Ds1)),
              V1 = ((erl_data_list_types@ps:uncons())(Cs1)),
              V2 = ((erl_data_list_types@ps:uncons())(Bs1)),
              V3 = ((erl_data_list_types@ps:uncons())(As1)),
              case (((erlang:is_tuple(V3)) andalso ((1 =< (erlang:tuple_size(V3))) andalso ((just =:= (erlang:element(1, V3))) andalso true))) andalso (((erlang:is_tuple(V2)) andalso ((1 =< (erlang:tuple_size(V2))) andalso ((just =:= (erlang:element(1, V2))) andalso true))) andalso (((erlang:is_tuple(V1)) andalso ((1 =< (erlang:tuple_size(V1))) andalso ((just =:= (erlang:element(1, V1))) andalso true))) andalso ((erlang:is_tuple(V)) andalso ((1 =< (erlang:tuple_size(V))) andalso ((just =:= (erlang:element(1, V))) andalso true)))))) of
                true ->
                  ((Go@Rec())([((((F((maps:get(head, (erlang:element(2, V3))))))((maps:get(head, (erlang:element(2, V2))))))((maps:get(head, (erlang:element(2, V1))))))((maps:get(head, (erlang:element(2, V))))))|Acc], (maps:get(tail, (erlang:element(2, V3)))), (maps:get(tail, (erlang:element(2, V2)))), (maps:get(tail, (erlang:element(2, V1)))), (maps:get(tail, (erlang:element(2, V))))));
                _ ->
                  ((erl_data_list@ps:reverse())(Acc))
              end
            end
        end)
    end),
    ((Go@Rec())([], As, Bs, Cs, Ds))
  end.
result() ->
  ((((((snapshot_fnXLazyBug@ps:zipWith4())((fun
    (A) ->
      (fun
        (B) ->
          (fun
            (C) ->
              (fun
                (D) ->
                  ((A * B) + (C * D))
              end)
          end)
      end)
  end)))([1,2,9]))([0,1]))([3,4]))([5,6,7,8])).

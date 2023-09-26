-module(snapshot_fnXLazyBug@ps).
-export([zipWith4/5, zipWith4/0, result/0]).
zipWith4(F, As@1, Bs@2, Cs@3, Ds@4) ->
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
              case ((just =:= (erlang:element(1, V3))) andalso ((just =:= (erlang:element(1, V2))) andalso ((just =:= (erlang:element(1, V1))) andalso (just =:= (erlang:element(1, V)))))) of
                true ->
                  ((Go@Rec())([((((F((maps:get(head, (erlang:element(2, V3))))))((maps:get(head, (erlang:element(2, V2))))))((maps:get(head, (erlang:element(2, V1))))))((maps:get(head, (erlang:element(2, V))))))|Acc], (maps:get(tail, (erlang:element(2, V3)))), (maps:get(tail, (erlang:element(2, V2)))), (maps:get(tail, (erlang:element(2, V1)))), (maps:get(tail, (erlang:element(2, V))))));
                _ ->
                  ((erl_data_list@ps:reverse())(Acc))
              end
            end
        end)
    end),
    ((Go@Rec())([], As@1, Bs@2, Cs@3, Ds@4))
  end.
zipWith4() ->
  (fun
    (F) ->
      (fun
        (As) ->
          (fun
            (Bs) ->
              (fun
                (Cs) ->
                  (fun
                    (Ds) ->
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
                                  case ((just =:= (erlang:element(1, V3))) andalso ((just =:= (erlang:element(1, V2))) andalso ((just =:= (erlang:element(1, V1))) andalso (just =:= (erlang:element(1, V)))))) of
                                    true ->
                                      ((Go@Rec())([((((F((maps:get(head, (erlang:element(2, V3))))))((maps:get(head, (erlang:element(2, V2))))))((maps:get(head, (erlang:element(2, V1))))))((maps:get(head, (erlang:element(2, V))))))|Acc], (maps:get(tail, (erlang:element(2, V3)))), (maps:get(tail, (erlang:element(2, V2)))), (maps:get(tail, (erlang:element(2, V1)))), (maps:get(tail, (erlang:element(2, V))))));
                                    _ ->
                                      ((erl_data_list@ps:reverse())(Acc))
                                  end
                                end
                            end)
                        end),
                        ((Go@Rec())([], As, Bs, Cs, Ds))
                      end
                  end)
              end)
          end)
      end)
  end).
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

-module(snapshot_fnXLazyBug@ps).
-export([zipWith4/0, result/0]).
zipWith4() ->
  (fun
    (F@0) ->
      (fun
        (As@1) ->
          (fun
            (Bs@2) ->
              (fun
                (Cs@3) ->
                  (fun
                    (Ds@4) ->
                      begin
                        Go@Rec@5 = (fun
                          Go@Rec@5 () ->
                            (fun
                              (Acc@6, As1@7, Bs1@8, Cs1@9, Ds1@10) ->
                                begin
                                  V@11 = ((erl_data_list_types@ps:uncons())(Ds1@10)),
                                  V1@12 = ((erl_data_list_types@ps:uncons())(Cs1@9)),
                                  V2@13 = ((erl_data_list_types@ps:uncons())(Bs1@8)),
                                  V3@14 = ((erl_data_list_types@ps:uncons())(As1@7)),
                                  case (just =:= (erlang:element(1, V3@14))) of
                                    true ->
                                      case (just =:= (erlang:element(1, V2@13))) of
                                        true ->
                                          case (just =:= (erlang:element(1, V1@12))) of
                                            true ->
                                              case (just =:= (erlang:element(1, V@11))) of
                                                true ->
                                                  ((Go@Rec@5())([((((F@0((maps:get(head, (erlang:element(2, V3@14))))))((maps:get(head, (erlang:element(2, V2@13))))))((maps:get(head, (erlang:element(2, V1@12))))))((maps:get(head, (erlang:element(2, V@11))))))|Acc@6], (maps:get(tail, (erlang:element(2, V3@14)))), (maps:get(tail, (erlang:element(2, V2@13)))), (maps:get(tail, (erlang:element(2, V1@12)))), (maps:get(tail, (erlang:element(2, V@11))))));
                                                _ ->
                                                  ((erl_data_list@ps:reverse())(Acc@6))
                                              end;
                                            _ ->
                                              ((erl_data_list@ps:reverse())(Acc@6))
                                          end;
                                        _ ->
                                          ((erl_data_list@ps:reverse())(Acc@6))
                                      end;
                                    _ ->
                                      ((erl_data_list@ps:reverse())(Acc@6))
                                  end
                                end
                            end)
                        end),
                        ((Go@Rec@5())([], As@1, Bs@2, Cs@3, Ds@4))
                      end
                  end)
              end)
          end)
      end)
  end).
result() ->
  ((((((zipWith4())((fun
    (A@0) ->
      (fun
        (B@1) ->
          (fun
            (C@2) ->
              (fun
                (D@3) ->
                  ((A@0 * B@1) + (C@2 * D@3))
              end)
          end)
      end)
  end)))([1,2,9]))([0,1]))([3,4]))([5,6,7,8])).

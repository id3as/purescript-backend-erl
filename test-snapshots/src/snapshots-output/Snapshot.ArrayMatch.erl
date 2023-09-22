-module(snapshot_arrayMatch@ps).
-export([onlyArray/0, nestedArrayViaRecord/0, nestedArrayRefutable2/0, nestedArrayRefutable/0, nestedArray/0, namedArray/0, maybeArray/0, bug28_2/0, bug28/0, result/0]).
onlyArray() ->
  (fun
    (A@0) ->
      case ((array:size(A@0)) =:= 2) of
        true ->
          ((array:get(0, A@0)) + (array:get(1, A@0)));
        _ ->
          0
      end
  end).
nestedArrayViaRecord() ->
  (fun
    (V@0) ->
      case ((array:size((maps:get(q, V@0)))) =:= 1) of
        true ->
          case ((array:size((maps:get(r, (array:get(0, (maps:get(q, V@0)))))))) =:= 2) of
            true ->
              ((array:get(0, (maps:get(r, (array:get(0, (maps:get(q, V@0)))))))) + (array:get(1, (maps:get(r, (array:get(0, (maps:get(q, V@0)))))))));
            _ ->
              0
          end;
        _ ->
          case (((array:size((maps:get(q, V@0)))) =:= 2) andalso (((array:size((maps:get(r, (array:get(0, (maps:get(q, V@0)))))))) =:= 1) andalso ((array:size((maps:get(r, (array:get(1, (maps:get(q, V@0)))))))) =:= 1))) of
            true ->
              ((array:get(0, (maps:get(r, (array:get(0, (maps:get(q, V@0)))))))) + (array:get(0, (maps:get(r, (array:get(1, (maps:get(q, V@0)))))))));
            _ ->
              0
          end
      end
  end).
nestedArrayRefutable2() ->
  (fun
    (V@0) ->
      (fun
        (V1@1) ->
          case (((array:size(V@0)) =:= 2) andalso (((array:size((array:get(0, V@0)))) =:= 3) andalso (((array:get(2, (array:get(0, V@0)))) =:= 3) andalso (V1@1 =:= 2)))) of
            true ->
              (((array:get(0, (array:get(0, V@0)))) + (array:get(1, (array:get(0, V@0))))) + 1);
            _ ->
              0
          end
      end)
  end).
nestedArrayRefutable() ->
  (fun
    (Arg1@0) ->
      (fun
        (Arg2@1) ->
          case (((array:size(Arg1@0)) =:= 2) andalso (((array:size((array:get(0, Arg1@0)))) =:= 3) andalso (((array:get(2, (array:get(0, Arg1@0)))) =:= 3) andalso (Arg2@1 =:= 2)))) of
            true ->
              (((array:get(0, (array:get(0, Arg1@0)))) + (array:get(1, (array:get(0, Arg1@0))))) + 1);
            _ ->
              0
          end
      end)
  end).
nestedArray() ->
  (fun
    (V@0) ->
      case (((array:size(V@0)) =:= 2) andalso ((array:size((array:get(0, V@0)))) =:= 2)) of
        true ->
          (((array:get(0, (array:get(0, V@0)))) + (array:get(1, (array:get(0, V@0))))) + 1);
        _ ->
          0
      end
  end).
namedArray() ->
  (fun
    (V@0) ->
      case ((array:size(V@0)) =:= 2) of
        true ->
          ((array:get(0, V@0)) + (array:get(1, V@0)));
        _ ->
          0
      end
  end).
maybeArray() ->
  (fun
    (A@0) ->
      case ((just =:= (erlang:element(1, A@0))) andalso ((array:size((erlang:element(2, A@0)))) =:= 2)) of
        true ->
          ((array:get(0, (erlang:element(2, A@0)))) + (array:get(1, (erlang:element(2, A@0)))));
        _ ->
          0
      end
  end).
bug28_2() ->
  (fun
    (A@0) ->
      3
  end).
bug28() ->
  (fun
    (A@0) ->
      case ((array:size((maps:get(q, A@0)))) =:= 2) of
        true ->
          ((array:get(0, (maps:get(q, A@0)))) + (array:get(1, (maps:get(q, A@0)))));
        _ ->
          0
      end
  end).
result() ->
  (array:from_list([((snapshot_arrayMatch@ps:bug28())(#{q => (array:from_list([1,2]))})),((snapshot_arrayMatch@ps:bug28_2())(#{q => (array:from_list([1,2]))})),((snapshot_arrayMatch@ps:nestedArray())((array:from_list([(array:from_list([1,2])),(array:from_list([3]))])))),((snapshot_arrayMatch@ps:nestedArrayViaRecord())(#{q => (array:from_list([#{r => (array:from_list([1,2]))},#{r => (array:from_list([3]))}]))})),((snapshot_arrayMatch@ps:onlyArray())((array:from_list([1])))),((snapshot_arrayMatch@ps:maybeArray())({just,(array:from_list([1,2]))})),((snapshot_arrayMatch@ps:namedArray())((array:from_list([1,2]))))])).

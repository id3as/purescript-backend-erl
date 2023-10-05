-module(snapshot_arrayMatch@ps).
-export([ onlyArray/0
        , onlyArray/1
        , nestedArrayViaRecord/0
        , nestedArrayViaRecord/1
        , nestedArrayRefutable2/0
        , nestedArrayRefutable2/2
        , nestedArrayRefutable/0
        , nestedArrayRefutable/2
        , nestedArray/0
        , nestedArray/1
        , namedArray/0
        , namedArray/1
        , maybeArray/0
        , maybeArray/1
        , bug28_2/0
        , bug28_2/1
        , bug28/0
        , bug28/1
        , result/0
        ]).
-compile(no_auto_import).
-define( IS_TAG(Tag, V)
       , ((erlang:is_tuple(V))
         andalso ((1 =< (erlang:tuple_size(V)))
           andalso (Tag =:= (erlang:element(1, V)))))
       ).
onlyArray() ->
  fun
    (A@Local) ->
      onlyArray(A@Local)
  end.

onlyArray(A) ->
  case (array:size(A)) =:= 2 of
    true ->
      (array:get(0, A)) + (array:get(1, A));
    _ ->
      0
  end.

nestedArrayViaRecord() ->
  fun
    (V@Local) ->
      nestedArrayViaRecord(V@Local)
  end.

nestedArrayViaRecord(V) ->
  case (array:size(erlang:map_get(q, V))) =:= 1 of
    true ->
      case (array:size(erlang:map_get(r, array:get(0, erlang:map_get(q, V)))))
          =:= 2 of
        true ->
          (array:get(0, erlang:map_get(r, array:get(0, erlang:map_get(q, V)))))
            + (array:get( 1
                        , erlang:map_get(r, array:get(0, erlang:map_get(q, V)))
                        ));
        _ ->
          0
      end;
    _ ->
      case ((array:size(erlang:map_get(q, V))) =:= 2)
          andalso (((array:size(erlang:map_get( r
                                              , array:get( 0
                                                         , erlang:map_get(q, V)
                                                         )
                                              )))
            =:= 1)
            andalso ((array:size(erlang:map_get( r
                                               , array:get( 1
                                                          , erlang:map_get(q, V)
                                                          )
                                               )))
              =:= 1)) of
        true ->
          (array:get(0, erlang:map_get(r, array:get(0, erlang:map_get(q, V)))))
            + (array:get( 0
                        , erlang:map_get(r, array:get(1, erlang:map_get(q, V)))
                        ));
        _ ->
          0
      end
  end.

nestedArrayRefutable2() ->
  fun
    (V@Local) ->
      fun
        (V1@Local@1) ->
          nestedArrayRefutable2(V@Local, V1@Local@1)
      end
  end.

nestedArrayRefutable2(V, V1) ->
  case ((array:size(V)) =:= 2)
      andalso (((array:size(array:get(0, V))) =:= 3)
        andalso (((array:get(2, array:get(0, V))) =:= 3) andalso (V1 =:= 2))) of
    true ->
      ((array:get(0, array:get(0, V))) + (array:get(1, array:get(0, V)))) + 1;
    _ ->
      0
  end.

nestedArrayRefutable() ->
  fun
    (Arg1@Local) ->
      fun
        (Arg2@Local@1) ->
          nestedArrayRefutable(Arg1@Local, Arg2@Local@1)
      end
  end.

nestedArrayRefutable(Arg1, Arg2) ->
  case ((array:size(Arg1)) =:= 2)
      andalso (((array:size(array:get(0, Arg1))) =:= 3)
        andalso (((array:get(2, array:get(0, Arg1))) =:= 3)
          andalso (Arg2 =:= 2))) of
    true ->
      ((array:get(0, array:get(0, Arg1))) + (array:get(1, array:get(0, Arg1))))
        + 1;
    _ ->
      0
  end.

nestedArray() ->
  fun
    (V@Local) ->
      nestedArray(V@Local)
  end.

nestedArray(V) ->
  case ((array:size(V)) =:= 2) andalso ((array:size(array:get(0, V))) =:= 2) of
    true ->
      ((array:get(0, array:get(0, V))) + (array:get(1, array:get(0, V)))) + 1;
    _ ->
      0
  end.

namedArray() ->
  fun
    (V@Local) ->
      namedArray(V@Local)
  end.

namedArray(V) ->
  case (array:size(V)) =:= 2 of
    true ->
      (array:get(0, V)) + (array:get(1, V));
    _ ->
      0
  end.

maybeArray() ->
  fun
    (A@Local) ->
      maybeArray(A@Local)
  end.

maybeArray(A) ->
  case ?IS_TAG(just, A) andalso ((array:size(erlang:element(2, A))) =:= 2) of
    true ->
      (array:get(0, erlang:element(2, A)))
        + (array:get(1, erlang:element(2, A)));
    _ ->
      0
  end.

bug28_2() ->
  fun
    (A@Local) ->
      bug28_2(A@Local)
  end.

bug28_2(_) ->
  3.

bug28() ->
  fun
    (A@Local) ->
      bug28(A@Local)
  end.

bug28(A) ->
  case (array:size(erlang:map_get(q, A))) =:= 2 of
    true ->
      (array:get(0, erlang:map_get(q, A)))
        + (array:get(1, erlang:map_get(q, A)));
    _ ->
      0
  end.

result() ->
  array:from_list([ (snapshot_arrayMatch@ps:bug28())
                    (#{ q => array:from_list([1, 2]) })
                  , (snapshot_arrayMatch@ps:bug28_2())
                    (#{ q => array:from_list([1, 2]) })
                  , (snapshot_arrayMatch@ps:nestedArray())
                    (array:from_list([ array:from_list([1, 2])
                                     , array:from_list([3])
                                     ]))
                  , (snapshot_arrayMatch@ps:nestedArrayViaRecord())
                    (#{ q =>
                        array:from_list([ #{ r => array:from_list([1, 2]) }
                                        , #{ r => array:from_list([3]) }
                                        ])
                      })
                  , (snapshot_arrayMatch@ps:onlyArray())(array:from_list([1]))
                  , (snapshot_arrayMatch@ps:maybeArray())
                    ({just, array:from_list([1, 2])})
                  , (snapshot_arrayMatch@ps:namedArray())
                    (array:from_list([1, 2]))
                  ]).


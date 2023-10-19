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
-define( IS_KNOWN_TAG(Tag, Arity, V)
       , ((erlang:is_tuple(V))
         andalso (((Arity + 1) =:= (erlang:tuple_size(V)))
           andalso (Tag =:= (erlang:element(1, V)))))
       ).
onlyArray() ->
  fun
    (A) ->
      onlyArray(A)
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
    (V) ->
      nestedArrayViaRecord(V)
  end.

nestedArrayViaRecord(V = #{ q := V@1 }) ->
  case (array:size(V@1)) =:= 1 of
    true ->
      case (array:size(erlang:map_get(r, array:get(0, V@1)))) =:= 2 of
        true ->
          begin
            #{ q := V@2 } = V,
            (array:get(0, erlang:map_get(r, array:get(0, V@2))))
              + (array:get(1, erlang:map_get(r, array:get(0, V@2))))
          end;
        _ ->
          0
      end;
    _ ->
      case ((array:size(V@1)) =:= 2)
          andalso (((array:size(erlang:map_get(r, array:get(0, V@1)))) =:= 1)
            andalso ((array:size(erlang:map_get(r, array:get(1, V@1)))) =:= 1)) of
        true ->
          begin
            #{ q := V@3 } = V,
            (array:get(0, erlang:map_get(r, array:get(0, V@3))))
              + (array:get(0, erlang:map_get(r, array:get(1, V@3))))
          end;
        _ ->
          0
      end
  end.

nestedArrayRefutable2() ->
  fun
    (V) ->
      fun
        (V1) ->
          nestedArrayRefutable2(V, V1)
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
    (Arg1) ->
      fun
        (Arg2) ->
          nestedArrayRefutable(Arg1, Arg2)
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
    (V) ->
      nestedArray(V)
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
    (V) ->
      namedArray(V)
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
    (A) ->
      maybeArray(A)
  end.

maybeArray(A) ->
  case ?IS_KNOWN_TAG(just, 1, A)
      andalso ((array:size(erlang:element(2, A))) =:= 2) of
    true ->
      begin
        {just, A@1} = A,
        (array:get(0, A@1)) + (array:get(1, A@1))
      end;
    _ ->
      0
  end.

bug28_2() ->
  fun
    (A) ->
      bug28_2(A)
  end.

bug28_2(_) ->
  3.

bug28() ->
  fun
    (A) ->
      bug28(A)
  end.

bug28(A = #{ q := A@1 }) ->
  case (array:size(A@1)) =:= 2 of
    true ->
      begin
        #{ q := A@2 } = A,
        (array:get(0, A@2)) + (array:get(1, A@2))
      end;
    _ ->
      0
  end.

result() ->
  array:from_list([ bug28(#{ q => array:from_list([1, 2]) })
                  , bug28_2(#{ q => array:from_list([1, 2]) })
                  , nestedArray(array:from_list([ array:from_list([1, 2])
                                                , array:from_list([3])
                                                ]))
                  , nestedArrayViaRecord(#{ q =>
                                            array:from_list([ #{ r =>
                                                                 array:from_list([ 1
                                                                                 , 2
                                                                                 ])
                                                               }
                                                            , #{ r =>
                                                                 array:from_list([3])
                                                               }
                                                            ])
                                          })
                  , onlyArray(array:from_list([1]))
                  , maybeArray({just, array:from_list([1, 2])})
                  , namedArray(array:from_list([1, 2]))
                  ]).


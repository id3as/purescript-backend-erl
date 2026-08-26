% Snapshot.ScrutineeFusion
-module(snapshot_scrutineeFusion@ps).
-export([ fromFoldable/0
        , assertEqual/0
        , assertEqual2/0
        , sumList/0
        , sumList/1
        , memoish/0
        , memoish/2
        , lookupNested/0
        , lookupNested/2
        , len/0
        , len/1
        , getOr0/0
        , getOr0/2
        , escapes/0
        , escapes/1
        , countJusts/0
        , countJusts/1
        , result/0
        , 'main.0'/0
        , main/0
        , fromFoldable/1
        ]).
-compile(no_auto_import).
-define( IS_KNOWN_TAG(Tag, Arity, V)
       , ((erlang:is_tuple(V))
           andalso (((Arity + 1) =:= (erlang:tuple_size(V)))
             andalso (Tag =:= (erlang:element(1, V)))))
       ).
-define( MEMOIZE_AS(Key, _Metadata, Expr)
       , case persistent_term:get(Key, undefined) of
           undefined ->
             begin
               MemoizeAsResult = Expr,
               persistent_term:put(Key, MemoizeAsResult),
               MemoizeAsResult
             end;
           MemoizeAsResult ->
             MemoizeAsResult
         end
       ).

fromFoldable() ->
  ((data_foldable@ps:foldrArray())(erl_data_list_types@ps:cons()))([]).

assertEqual() ->
  test_assert@ps:assertEqual(
    #{ eq =>
       fun
         (X) ->
           fun
             (Y) ->
               case X of
                 {nothing} ->
                   ?IS_KNOWN_TAG(nothing, 0, Y);
                 _ ->
                   ?IS_KNOWN_TAG(just, 1, X)
                     andalso (?IS_KNOWN_TAG(just, 1, Y)
                       andalso ((erlang:element(2, X))
                         =:= (erlang:element(2, Y))))
               end
           end
       end
     },
    #{ show =>
       fun
         (V) ->
           case V of
             {just, V@1} ->
               <<"(Just ", (data_show@foreign:showIntImpl(V@1))/binary, ")">>;
             {nothing} ->
               <<"Nothing">>;
             _ ->
               erlang:error({fail, <<"Failed pattern match">>})
           end
       end
     }
  ).

assertEqual2() ->
  ?MEMOIZE_AS(
    {snapshot_scrutineeFusion@ps, assertEqual2, '(memoized)'},
    25,
    test_assert@ps:assertEqual(
      #{ eq =>
         fun
           (X) ->
             fun
               (Y) ->
                 case X of
                   {nothing} ->
                     ?IS_KNOWN_TAG(nothing, 0, Y);
                   _ ->
                     ?IS_KNOWN_TAG(just, 1, X)
                       andalso (?IS_KNOWN_TAG(just, 1, Y)
                         andalso (((erlang:map_get(head, erlang:element(2, X)))
                           =:= (erlang:map_get(head, erlang:element(2, Y))))
                           andalso ((((erlang:map_get(
                                         eq1,
                                         erl_data_list_types@ps:eq1List()
                                       ))
                                      (data_eq@ps:eqInt()))
                                     (erlang:map_get(tail, erlang:element(2, X))))
                                    (erlang:map_get(tail, erlang:element(2, Y))))))
                 end
             end
         end
       },
      begin
        V@1 =
          data_show@ps:showRecord(
            undefined,
            begin
              #{ show := V } =
                erl_data_list_types@ps:showList(data_show@ps:showInt()),
              #{ showRecordFields =>
                 fun
                   (_) ->
                     fun
                       (#{ head := Record, tail := Record@1 }) ->
                         data_show@foreign:cons(
                           data_show@foreign:join(
                             <<": ">>,
                             array:from_list([ <<"head">>
                                             , data_show@foreign:showIntImpl(Record)
                                             ])
                           ),
                           data_show@foreign:cons(
                             data_show@foreign:join(
                               <<": ">>,
                               array:from_list([<<"tail">>, V(Record@1)])
                             ),
                             array:from_list([])
                           )
                         )
                     end
                 end
               }
            end
          ),
        #{ show =>
           fun
             (V@2) ->
               case V@2 of
                 {just, V@3} ->
                   begin
                     #{ show := V@4 } = V@1,
                     <<"(Just ", (V@4(V@3))/binary, ")">>
                   end;
                 {nothing} ->
                   <<"Nothing">>;
                 _ ->
                   erlang:error({fail, <<"Failed pattern match">>})
               end
           end
         }
      end
    )
  ).

sumList() ->
  fun sumList/1.

sumList(L) ->
  case L of
    [] ->
      0;
    [ V | V@1 ] ->
      V + (sumList(V@1));
    _ ->
      erlang:error({fail, <<"Failed pattern match">>})
  end.

memoish() ->
  fun
    (M) ->
      fun
        (K) ->
          memoish(M, K)
      end
  end.

memoish(M, K) ->
  begin
    V = erl_data_map@ps:lookup(K, M),
    case V of
      {nothing} ->
        {nothing};
      _ ->
        V
    end
  end.

lookupNested() ->
  fun
    (M) ->
      fun
        (K) ->
          lookupNested(M, K)
      end
  end.

lookupNested(M, K) ->
  case maps:find(K, M) of
    {ok, V} ->
      if
        (erlang:element(3, V)) =:= 0 ->
          erlang:element(2, V);
        true ->
          erlang:element(3, V)
      end;
    error ->
      -1;
    _ ->
      erlang:error({fail, <<"Failed pattern match">>})
  end.

len() ->
  fun len/1.

len(L) ->
  case L of
    [] ->
      0;
    [ _ | V ] ->
      1 + (len(V));
    _ ->
      erlang:error({fail, <<"Failed pattern match">>})
  end.

getOr0() ->
  fun
    (M) ->
      fun
        (K) ->
          getOr0(M, K)
      end
  end.

getOr0(M, K) ->
  case maps:find(K, M) of
    error ->
      0;
    {ok, V} ->
      V;
    _ ->
      erlang:error({fail, <<"Failed pattern match">>})
  end.

escapes() ->
  fun escapes/1.

escapes(L) ->
  begin
    V = erl_data_list_types@ps:uncons(L),
    case V of
      {nothing} ->
        {nothing};
      _ ->
        V
    end
  end.

countJusts() ->
  fun countJusts/1.

countJusts(L) ->
  case L of
    [] ->
      0;
    [ V | V@1 ] ->
      case V of
        {just, _} ->
          1 + (countJusts(V@1));
        {nothing} ->
          countJusts(V@1);
        _ ->
          erlang:error({fail, <<"Failed pattern match">>})
      end;
    _ ->
      erlang:error({fail, <<"Failed pattern match">>})
  end.

result() ->
  ?MEMOIZE_AS(
    {snapshot_scrutineeFusion@ps, result, '(memoized)'},
    86,
    #{ sumList => sumList(fromFoldable(array:from_list([1, 2, 3])))
     , countJusts =>
       countJusts(fromFoldable(array:from_list([{just, 1}, {nothing}, {just, 3}])))
     , len => len(fromFoldable(array:from_list([unit, unit])))
     , escapes =>
       begin
         V = erl_data_list_types@ps:uncons(fromFoldable(array:from_list([7]))),
         case V of
           {nothing} ->
             {nothing};
           _ ->
             V
         end
       end
     , getOr0 =>
       case maps:find(a, #{}) of
         error ->
           0;
         {ok, V@1} ->
           V@1;
         _ ->
           erlang:error({fail, <<"Failed pattern match">>})
       end
     , lookupNested => lookupNested(#{ a => {tuple, 4, 5} }, a)
     }
  ).

'main.0'() ->
  ?MEMOIZE_AS(
    {snapshot_scrutineeFusion@ps, 'main.0', '(memoized)'},
    23,
    (assertEqual())
    (#{ expected => {just, 9}
      , actual =>
        begin
          V = erl_data_map@ps:lookup(m, #{ m => 9 }),
          case V of
            {nothing} ->
              {nothing};
            _ ->
              V
          end
        end
      })
  ).

main() ->
  fun
    () ->
      begin
        V = data_show@ps:showInt(),
        V@1 = data_eq@ps:eqInt(),
        V@2 = result(),
        ('main.0'())(),
        ((assertEqual())
         (#{ expected => {nothing}
           , actual =>
             begin
               V@3 = erl_data_map@ps:lookup(q, #{ m => 9 }),
               case V@3 of
                 {nothing} ->
                   {nothing};
                 _ ->
                   V@3
               end
             end
           }))(),
        (test_assert@ps:'assertEqual\''(
           V@1,
           V,
           <<"">>,
           #{ expected => 6, actual => erlang:map_get(sumList, V@2) }
         ))(),
        (test_assert@ps:'assertEqual\''(
           V@1,
           V,
           <<"">>,
           #{ expected => 2, actual => erlang:map_get(countJusts, V@2) }
         ))(),
        (test_assert@ps:'assertEqual\''(
           V@1,
           V,
           <<"">>,
           #{ expected => 2, actual => erlang:map_get(len, V@2) }
         ))(),
        ((assertEqual2())
         (#{ expected => {just, #{ head => 7, tail => [] }}
           , actual => erlang:map_get(escapes, V@2)
           }))(),
        (test_assert@ps:'assertEqual\''(
           V@1,
           V,
           <<"">>,
           #{ expected => 0, actual => erlang:map_get(getOr0, V@2) }
         ))(),
        (test_assert@ps:'assertEqual\''(
           V@1,
           V,
           <<"">>,
           #{ expected => 5, actual => erlang:map_get(lookupNested, V@2) }
         ))(),
        (test_assert@ps:'assertEqual\''(
           V@1,
           V,
           <<"">>,
           #{ expected => 4
            , actual => lookupNested(#{ b => {tuple, 4, 0} }, b)
            }
         ))(),
        (test_assert@ps:'assertEqual\''(
           V@1,
           V,
           <<"">>,
           #{ expected => -1
            , actual => lookupNested(#{ b => {tuple, 4, 0} }, c)
            }
         ))()
      end
  end.

fromFoldable(V) ->
  data_foldable@foreign:foldrArray(erl_data_list_types@ps:cons(), [], V).


% Snapshot.PatternGuardDemand
-module(snapshot_patternGuardDemand@ps).
-export([ any/0
        , scanFlip/0
        , scanFlip/2
        , processE/0
        , processE/3
        , baseState/0
        , basePayload/0
        , assertEq/0
        , assertEq/3
        , 'main.0'/0
        , main/0
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

any() ->
  ?MEMOIZE_AS(
    {snapshot_patternGuardDemand@ps, any, '(memoized)'},
    16,
    (erlang:map_get(foldMap, data_foldable@ps:foldableArray()))
    (begin
      SemigroupDisj1 =
        #{ append =>
           fun
             (V) ->
               fun
                 (V1) ->
                   V orelse V1
               end
           end
         },
      #{ mempty => false
       , 'Semigroup0' =>
         fun
           (_) ->
             SemigroupDisj1
         end
       }
    end)
  ).

scanFlip() ->
  fun
    (S) ->
      fun
        (V) ->
          scanFlip(S, V)
      end
  end.

scanFlip(S, _) ->
  if
    (erlang:map_get(metrics, S)) > 10 ->
      {just, 1};
    true ->
      {nothing}
  end.

processE() ->
  fun
    (V) ->
      fun
        (V1) ->
          fun
            (V2) ->
              processE(V, V1, V2)
          end
      end
  end.

processE( V
        , V1 = #{ dropping := V1@1
                , metrics := V1@2
                , pending := V1@3
                , pipeline := V1@4
                }
        , V2 = #{ payload := #{ p := V2@1, pts := V2@2 } }
        ) ->
  begin
    V@1 =
      fun
        () ->
          begin
            V3 =
              fun
                (_) ->
                  case V1@4 of
                    {just, _} ->
                      begin
                        V@1 =
                          if
                            erlang:map_get(gpu, erlang:element(2, V1@4)) ->
                              begin
                                #{ pipeline := V1@5 } = V1,
                                fun
                                  () ->
                                    erlang:map_get(
                                      refs,
                                      erlang:element(2, V1@5)
                                    )
                                end
                              end;
                            true ->
                              fun
                                () ->
                                  0
                              end
                          end,
                        fun
                          () ->
                            begin
                              Buf = V@1(),
                              (effect_ref@ps:modify_(
                                 fun
                                   (V5) ->
                                     (V5 + V1@2) + V1@3
                                 end,
                                 V
                               ))(),
                              (effect_ref@ps:modify_(
                                 fun
                                   (V5) ->
                                     V5 + V1@1
                                 end,
                                 V
                               ))(),
                              (Buf + V2@1) + V2@2
                            end
                        end
                      end;
                    _ ->
                      erlang:error({fail, <<"Failed pattern match">>})
                  end
              end,
            case V1@4 of
              {just, _} ->
                begin
                  #{ source := V2@3 } = V2,
                  #{ pipeline := {just, _} } = V1,
                  V@1 = scanFlip(V1, V2@3),
                  case V@1 of
                    {just, V@2} ->
                      begin
                        #{ payload := #{ p := V2@4 } } = V2,
                        #{ held := V1@5
                         , metrics := V1@6
                         , pending := V1@7
                         , pipeline := _
                         } =
                          V1,
                        V@3 =
                          effect_ref@ps:modify_(
                            fun
                              (V4) ->
                                V4 + V1@7
                            end,
                            V
                          ),
                        fun
                          () ->
                            begin
                              V@3(),
                              (effect_ref@ps:modify_(
                                 fun
                                   (V4) ->
                                     V4 + V1@5
                                 end,
                                 V
                               ))(),
                              (effect_ref@ps:modify_(
                                 fun
                                   (V4) ->
                                     V4 + V1@6
                                 end,
                                 V
                               ))(),
                              R =
                                (processE(
                                   V,
                                   V1#{ pipeline => {nothing}, metrics => 0 },
                                   V2
                                 ))(),
                              (R + V@2) + V2@4
                            end
                        end
                      end;
                    _ ->
                      V3(true)
                  end
                end;
              _ ->
                V3(true)
            end
          end
      end,
    case V1@4 of
      {nothing} ->
        case erlang:map_get(meta, V1) of
          {nothing} ->
            fun
              () ->
                0
            end;
          {just, _} ->
            begin
              #{ payload := #{ p := V2@3 } } = V2,
              #{ meta := {just, V1@5}, pipeline := _ } = V1,
              V@2 = V1@5 + V2@3,
              fun
                () ->
                  V@2
              end
            end;
          _ ->
            V@1()
        end;
      _ ->
        case ?IS_KNOWN_TAG(just, 1, V1@4)
            andalso (not (((any())
                           (fun
                             (V3) ->
                               V3 =:= (erlang:map_get(source, V2))
                           end))
                          (erlang:map_get(order, V1)))) of
          true ->
            fun
              () ->
                1
            end;
          _ ->
            V@1()
        end
    end
  end.

baseState() ->
  #{ pipeline => {nothing}
   , meta => {nothing}
   , order => array:from_list([<<"s">>])
   , pending => 1
   , held => 2
   , metrics => 3
   , dropping => 4
   }.

basePayload() ->
  #{ source => <<"s">>, payload => #{ p => 10, pts => 100 } }.

assertEq() ->
  fun
    (Label) ->
      fun
        (X) ->
          fun
            (Y) ->
              assertEq(Label, X, Y)
          end
      end
  end.

assertEq(Label, X, Y) ->
  if
    X =/= Y ->
      fun
        () ->
          erlang:error(effect_exception@foreign:error(<<
                         Label/binary,
                         ": got ",
                         (data_show@foreign:showIntImpl(X))/binary,
                         ", expected ",
                         (data_show@foreign:showIntImpl(Y))/binary
                       >>))
      end;
    true ->
      fun
        () ->
          unit
      end
  end.

'main.0'() ->
  effect_ref@foreign:new(0).

main() ->
  fun
    () ->
      begin
        V = baseState(),
        V@1 = basePayload(),
        Ref = ('main.0'())(),
        A = (processE(Ref, V, V@1))(),
        (assertEq(<<"nothing/nothing">>, A, 0))(),
        B = (processE(Ref, V#{ meta => {just, 5} }, V@1))(),
        (assertEq(<<"nothing/just">>, B, 15))(),
        C =
          (processE(
             Ref,
             V#{ pipeline => {just, #{ gpu => false, refs => 7 }} },
             V@1#{ source => <<"other">> }
           ))(),
        (assertEq(<<"unknown source">>, C, 1))(),
        D =
          (processE(
             Ref,
             V#{ pipeline => {just, #{ gpu => true, refs => 7 }} },
             V@1
           ))(),
        (assertEq(<<"steady gpu">>, D, 117))(),
        E =
          (processE(
             Ref,
             V#{ pipeline => {just, #{ gpu => false, refs => 7 }}
               , metrics => 11
               },
             V@1
           ))(),
        (assertEq(<<"scan flip">>, E, 11))()
      end
  end.


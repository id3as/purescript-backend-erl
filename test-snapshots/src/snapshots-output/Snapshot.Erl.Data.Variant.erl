% Snapshot.Erl.Data.Variant
-module(snapshot_erl_data_variant@ps).
-export([ showArray/0
        , show2/0
        , test3/0
        , test3/1
        , test2/0
        , test2/1
        , test1/0
        , test1/1
        , noInline/0
        , noInline/1
        , demandAnalysisBug/0
        , demandAnalysisBug/1
        , result/0
        ]).
-compile(no_auto_import).
-define( MEMOIZE_AS(Key, Expr)
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

showArray() ->
  #{ show =>
     (data_show@ps:showArrayImpl())
     (erlang:map_get(show, data_unit@ps:showUnit()))
   }.

show2() ->
  ?MEMOIZE_AS(
    {snapshot_erl_data_variant@ps, show2, '(memoized)'},
    erlang:map_get(
      show,
      erl_data_variant@ps:showVariant(
        undefined,
        #{ variantTags =>
           fun
             (_) ->
               {cons, a, {cons, b, {cons, c, {nil}}}}
           end
         },
        #{ variantShows =>
           fun
             (_) ->
               { cons
               , data_show@ps:showIntImpl()
               , { cons
                 , data_show@ps:showStringImpl()
                 , {cons, erlang:map_get(show, showArray()), {nil}}
                 }
               }
           end
         }
      )
    )
  ).

test3() ->
  fun test3/1.

test3(R = #{ type := R@1 }) ->
  if
    R@1 =:= c ->
      (erlang:map_get(show, showArray()))(erlang:map_get(value, R));
    R@1 =:= b ->
      data_show@foreign:showStringImpl(erlang:map_get(value, R));
    R@1 =:= a ->
      data_show@foreign:showIntImpl(erlang:map_get(value, R));
    true ->
      erlang:error(<<
        "Data.Variant: pattern match failure [atom(",
        (erlang:atom_to_binary(R@1))/binary,
        ")]"
      >>)
  end.

test2() ->
  fun test2/1.

test2(_) ->
  fun
    (#{ type := a, value := Value }) ->
      fun
        (#{ type := a, value := Value@1 }) ->
          { data_show@foreign:showIntImpl(Value)
          , data_show@foreign:showIntImpl(Value@1)
          };
        (#{ type := b, value := Value@1 }) ->
          {data_show@foreign:showIntImpl(Value), Value@1};
        (#{ type := c, value := Value@1 }) ->
          { data_show@foreign:showIntImpl(Value)
          , (erlang:map_get(show, showArray()))(Value@1)
          }
      end;
    (#{ type := b, value := Value }) ->
      begin
        V =
          fun
            (#{ type := a, value := Value@1 }) ->
              fun
                (B1) ->
                  {B1, data_show@foreign:showIntImpl(Value@1)}
              end;
            (#{ type := b, value := Value@1 }) ->
              fun
                (B1) ->
                  {B1, Value@1}
              end;
            (#{ type := c, value := Value@1 }) ->
              fun
                (B1) ->
                  {B1, (erlang:map_get(show, showArray()))(Value@1)}
              end
          end,
        fun
          (A) ->
            (V(A))(Value)
        end
      end;
    (#{ type := c, value := Value }) ->
      fun
        (V2) ->
          {(erlang:map_get(show, showArray()))(Value), (show2())(V2)}
      end
  end.

test1() ->
  fun test1/1.

test1(_) ->
  fun
    (#{ type := a, value := Value }) ->
      if
        Value =:= 5 ->
          6;
        true ->
          erlang:error({fail, <<"Failed pattern match">>})
      end;
    (#{ type := b, value := Value }) ->
      if
        Value =:= <<"5">> ->
          7;
        true ->
          erlang:error({fail, <<"Failed pattern match">>})
      end;
    (#{ type := c, value := Value }) ->
      case (array:size(Value)) =:= 5 of
        true ->
          8;
        _ ->
          erlang:error({fail, <<"Failed pattern match">>})
      end
  end.

noInline() ->
  fun noInline/1.

noInline(A) ->
  A.

demandAnalysisBug() ->
  fun demandAnalysisBug/1.

demandAnalysisBug(R = #{ type := R@1 }) ->
  (R@1 =:= a) andalso ((erlang:map_get(nested, erlang:map_get(value, R))) =:= 2).

result() ->
  ?MEMOIZE_AS(
    {snapshot_erl_data_variant@ps, result, '(memoized)'},
    { { (noInline(test1(undefined)))(#{ type => a, value => 5 })
      , (noInline(test1(undefined)))(#{ type => b, value => <<"5">> })
      , (noInline(test1(undefined)))
        (#{ type => c
          , value => array:from_list([unit, unit, unit, unit, unit])
          })
      }
    , ((noInline(test2(undefined)))(#{ type => a, value => 1 }))
      (#{ type => a, value => 2 })
    , { (noInline(fun test3/1))(#{ type => a, value => 5 })
      , (noInline(fun test3/1))(#{ type => b, value => <<"hi">> })
      , (noInline(fun test3/1))
        (#{ type => c, value => array:from_list([unit, unit, unit]) })
      }
    , { (noInline(fun demandAnalysisBug/1))
        (#{ type => a, value => #{ nested => 2 } })
      , (noInline(fun demandAnalysisBug/1))
        (#{ type => a, value => #{ nested => 3 } })
      , (noInline(fun demandAnalysisBug/1))(#{ type => b, value => unit })
      }
    }
  ).


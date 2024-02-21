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
        , result/0
        ]).
-compile(no_auto_import).
showArray() ->
  #{ show =>
     (data_show@ps:showArrayImpl())
     (erlang:map_get(show, data_unit@ps:showUnit()))
   }.

show2() ->
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
  ).

test3() ->
  fun
    (R) ->
      test3(R)
  end.

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
  fun
    (V) ->
      test2(V)
  end.

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
  fun
    (V) ->
      test1(V)
  end.

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
  fun
    (A) ->
      noInline(A)
  end.

noInline(A) ->
  A.

result() ->
  begin
    V = test3(),
    { { (noInline(test1(undefined)))(#{ type => a, value => 5 })
      , (noInline(test1(undefined)))(#{ type => b, value => <<"5">> })
      , (noInline(test1(undefined)))
        (#{ type => c
          , value => array:from_list([unit, unit, unit, unit, unit])
          })
      }
    , ((noInline(test2(undefined)))(#{ type => a, value => 1 }))
      (#{ type => a, value => 2 })
    , { (noInline(V))(#{ type => a, value => 5 })
      , (noInline(V))(#{ type => b, value => <<"hi">> })
      , (noInline(V))
        (#{ type => c, value => array:from_list([unit, unit, unit]) })
      }
    }
  end.


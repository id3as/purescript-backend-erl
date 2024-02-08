-module(test).
-compile(export_all).

eqVariant1() ->
  #{ eq =>
     fun
       (V1) ->
         fun
           (V2) ->
             begin
               V = erlang:map_get(eq, data_eq@ps:eqUnit()),
               erl_data_variant_internal@ps:lookupEq(
                 { cons
                 , fps_23_976
                 , { cons
                   , fps_24
                   , { cons
                     , fps_25
                     , { cons
                       , fps_29_97
                       , { cons
                         , fps_30
                         , { cons
                           , fps_50
                           , { cons
                             , fps_59_94
                             , { cons
                               , fps_60
                               , { cons
                                 , fps_otherFixed
                                 , {cons, fps_variable, {nil}}
                                 }
                               }
                             }
                           }
                         }
                       }
                     }
                   }
                 },
                 { cons
                 , V
                 , { cons
                   , V
                   , { cons
                     , V
                     , { cons
                       , V
                       , { cons
                         , V
                         , { cons
                           , V
                           , { cons
                             , V
                             , { cons
                               , V
                               , { cons
                                 , erlang:map_get(
                                     eq,
                                     common_time@ps:eqFixedFrameRate()
                                   )
                                 , {cons, V, {nil}}
                                 }
                               }
                             }
                           }
                         }
                       }
                     }
                   }
                 },
                 V1,
                 V2
               )
             end
         end
     end
   }.

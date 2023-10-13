-module(snapshot_let_goArity@ps).
-export([foldr/0, foldr/2, result/0]).
-compile(no_auto_import).
-define( IS_KNOWN_TAG(Tag, Arity, V)
       , ((erlang:is_tuple(V))
         andalso ((1 =< (erlang:tuple_size(V)))
           andalso (Tag =:= (erlang:element(1, V)))))
       ).
foldr() ->
  fun
    (F@Local) ->
      fun
        (B@Local@1) ->
          foldr(F@Local, B@Local@1)
      end
  end.

foldr(F, B) ->
  begin
    Go = fun
      Go (B@1, V) ->
        if
          ?IS_KNOWN_TAG(nil, 0, V) ->
            B@1;
          ?IS_KNOWN_TAG(cons, 2, V) ->
            begin
              {_, V@@0, V@@1} = V,
              ((fun
                 (B@Local) ->
                   fun
                     (V@Local@1) ->
                       Go(B@Local, V@Local@1)
                   end
               end)
               ((F(V@@0))(B@1)))
              (V@@1)
            end;
          true ->
            erlang:throw({fail, <<"Failed pattern match">>})
        end
    end,
    V = (fun
          (B@Local) ->
            fun
              (V@Local@1) ->
                Go(B@Local, V@Local@1)
            end
        end)
        (B),
    Go@1 = fun
      Go@1 (V@1, V1) ->
        if
          ?IS_KNOWN_TAG(nil, 0, V1) ->
            V@1;
          ?IS_KNOWN_TAG(cons, 2, V1) ->
            begin
              {_, V1@@0, V1@@1} = V1,
              ((fun
                 (V@Local) ->
                   fun
                     (V1@Local@1) ->
                       Go@1(V@Local, V1@Local@1)
                   end
               end)
               ({cons, V1@@0, V@1}))
              (V1@@1)
            end;
          true ->
            erlang:throw({fail, <<"Failed pattern match">>})
        end
    end,
    V@1 = (fun
            (V@Local) ->
              fun
                (V1@Local@1) ->
                  Go@1(V@Local, V1@Local@1)
              end
          end)
          ({nil}),
    fun
      (X) ->
        V(V@1(X))
    end
  end.

result() ->
  (foldr(data_semiring@ps:intAdd(), 1))({cons, 2, {cons, 3, {cons, 4, {nil}}}}).


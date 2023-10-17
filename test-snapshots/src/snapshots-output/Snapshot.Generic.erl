-module(snapshot_generic@ps).
-export(['A'/0, 'D'/0, 'T'/0, genericADT/0]).
-compile(no_auto_import).
-define( IS_KNOWN_TAG(Tag, Arity, V)
       , ((erlang:is_tuple(V))
         andalso ((1 =< (erlang:tuple_size(V)))
           andalso (Tag =:= (erlang:element(1, V)))))
       ).
'A'() ->
  {a}.

'D'() ->
  fun
    (Value0) ->
      {d, Value0}
  end.

'T'() ->
  fun
    (Value0) ->
      fun
        (Value1) ->
          {t, Value0, Value1}
      end
  end.

genericADT() ->
  #{ to =>
     fun
       (X) ->
         if
           ?IS_KNOWN_TAG(inl, 1, X) ->
             {a};
           ?IS_KNOWN_TAG(inr, 1, X) ->
             if
               ?IS_KNOWN_TAG(inl, 1, erlang:element(2, X)) ->
                 {d, erlang:element(2, erlang:element(2, X))};
               ?IS_KNOWN_TAG(inr, 1, erlang:element(2, X)) ->
                 { t
                 , erlang:element(2, erlang:element(2, erlang:element(2, X)))
                 , erlang:element(3, erlang:element(2, erlang:element(2, X)))
                 };
               true ->
                 erlang:throw({fail, <<"Failed pattern match">>})
             end;
           true ->
             erlang:throw({fail, <<"Failed pattern match">>})
         end
     end
   , from =>
     fun
       (X) ->
         if
           ?IS_KNOWN_TAG(a, 0, X) ->
             {inl, {noArguments}};
           ?IS_KNOWN_TAG(d, 1, X) ->
             {inr, {inl, erlang:element(2, X)}};
           ?IS_KNOWN_TAG(t, 2, X) ->
             begin
               {_, X@1, X@2} = X,
               {inr, {inr, {product, X@1, X@2}}}
             end;
           true ->
             erlang:throw({fail, <<"Failed pattern match">>})
         end
     end
   }.


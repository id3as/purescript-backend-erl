-module(snapshot_generic@ps).
-export([ 'E1'/0
        , 'E2'/0
        , 'E3'/0
        , 'E4'/0
        , 'E5'/0
        , 'E6'/0
        , 'E7'/0
        , 'E8'/0
        , 'A'/0
        , 'D'/0
        , 'T'/0
        , genericAnEnum/0
        , genericADT/0
        ]).
-compile(no_auto_import).
'E1'() ->
  {e1}.

'E2'() ->
  {e2}.

'E3'() ->
  {e3}.

'E4'() ->
  {e4}.

'E5'() ->
  {e5}.

'E6'() ->
  {e6}.

'E7'() ->
  {e7}.

'E8'() ->
  {e8}.

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

genericAnEnum() ->
  #{ to =>
     fun
       (X) ->
         case X of
           {inl, _} ->
             {e1};
           {inr, X@1} ->
             case X@1 of
               {inl, _} ->
                 {e2};
               {inr, X@2} ->
                 case X@2 of
                   {inl, _} ->
                     {e3};
                   {inr, X@3} ->
                     case X@3 of
                       {inl, _} ->
                         {e4};
                       {inr, X@4} ->
                         case X@4 of
                           {inl, _} ->
                             {e5};
                           {inr, X@5} ->
                             case X@5 of
                               {inl, _} ->
                                 {e6};
                               {inr, X@6} ->
                                 case X@6 of
                                   {inl, _} ->
                                     {e7};
                                   {inr, _} ->
                                     {e8};
                                   _ ->
                                     erlang:error({ fail
                                                  , <<"Failed pattern match">>
                                                  })
                                 end;
                               _ ->
                                 erlang:error({fail, <<"Failed pattern match">>})
                             end;
                           _ ->
                             erlang:error({fail, <<"Failed pattern match">>})
                         end;
                       _ ->
                         erlang:error({fail, <<"Failed pattern match">>})
                     end;
                   _ ->
                     erlang:error({fail, <<"Failed pattern match">>})
                 end;
               _ ->
                 erlang:error({fail, <<"Failed pattern match">>})
             end;
           _ ->
             erlang:error({fail, <<"Failed pattern match">>})
         end
     end
   , from =>
     fun
       (X) ->
         case X of
           {e1} ->
             {inl, {noArguments}};
           {e2} ->
             {inr, {inl, {noArguments}}};
           {e3} ->
             {inr, {inr, {inl, {noArguments}}}};
           {e4} ->
             {inr, {inr, {inr, {inl, {noArguments}}}}};
           {e5} ->
             {inr, {inr, {inr, {inr, {inl, {noArguments}}}}}};
           {e6} ->
             {inr, {inr, {inr, {inr, {inr, {inl, {noArguments}}}}}}};
           {e7} ->
             {inr, {inr, {inr, {inr, {inr, {inr, {inl, {noArguments}}}}}}}};
           {e8} ->
             {inr, {inr, {inr, {inr, {inr, {inr, {inr, {noArguments}}}}}}}};
           _ ->
             erlang:error({fail, <<"Failed pattern match">>})
         end
     end
   }.

genericADT() ->
  #{ to =>
     fun
       (X) ->
         case X of
           {inl, _} ->
             {a};
           {inr, X@1} ->
             case X@1 of
               {inl, X@2} ->
                 {d, X@2};
               {inr, X@3} ->
                 {t, erlang:element(2, X@3), erlang:element(3, X@3)};
               _ ->
                 erlang:error({fail, <<"Failed pattern match">>})
             end;
           _ ->
             erlang:error({fail, <<"Failed pattern match">>})
         end
     end
   , from =>
     fun
       (X) ->
         case X of
           {a} ->
             {inl, {noArguments}};
           {d, X@1} ->
             {inr, {inl, X@1}};
           {t, X@2, X@3} ->
             {inr, {inr, {product, X@2, X@3}}};
           _ ->
             erlang:error({fail, <<"Failed pattern match">>})
         end
     end
   }.


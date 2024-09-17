% Snapshot.Foreign
-module(snapshot_foreign@ps).
-export([ memptys/0
        , exceptions/0
        , coercions/0
        , byteSizes/0
        , atomId/0
        , atomEq/0
        , appends/0
        , atomId/1
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

memptys() ->
  #{ string => <<"">>, binary => <<"">>, iolist => [], iodata => [] }.

exceptions() ->
  ?MEMOIZE_AS(
    {snapshot_foreign@ps, exceptions, '(memoized)'},
    #{ throw =>
       fun
         () ->
           erlang:throw(<<"throw">>)
       end
     , error =>
       fun
         () ->
           erlang:error(<<"error">>)
       end
     , exit =>
       fun
         () ->
           erlang:exit(<<"exit">>)
       end
     , throwException =>
       fun
         () ->
           erlang:error(effect_exception@foreign:error(<<"throwException">>))
       end
     }
  ).

coercions() ->
  #{ binary2bitstring =>
     fun
       (B) ->
         B
     end
   , iolist2iodata =>
     fun
       (L) ->
         L
     end
   }.

byteSizes() ->
  ?MEMOIZE_AS(
    {snapshot_foreign@ps, byteSizes, '(memoized)'},
    #{ binary =>
       fun
         (B) ->
           erlang:byte_size(B)
       end
     , bitstring =>
       fun
         (B) ->
           erlang:byte_size(B)
       end
     , iodata =>
       fun
         (D) ->
           erlang:iolist_size(D)
       end
     , iolist =>
       fun
         (L) ->
           erlang:iolist_size(L)
       end
     }
  ).

atomId() ->
  fun
    ({S, A}) ->
      {S, A}
  end.

atomEq() ->
  #{ true => true, false => false }.

appends() ->
  ?MEMOIZE_AS(
    {snapshot_foreign@ps, appends, '(memoized)'},
    #{ string =>
       fun
         (A) ->
           fun
             (B) ->
               <<A/binary, B/binary>>
           end
       end
     , binary =>
       fun
         (A) ->
           fun
             (B) ->
               <<A/binary, B/binary>>
           end
       end
     , iolist =>
       fun
         (A) ->
           fun
             (B) ->
               [A, B]
           end
       end
     , iodata =>
       fun
         (A) ->
           fun
             (B) ->
               [A, B]
           end
       end
     }
  ).

atomId(V) ->
  begin
    {S, A} = V,
    {S, A}
  end.


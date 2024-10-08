% Snapshot.InlineCommonOperators
-module(snapshot_inlineCommonOperators@ps).
-export([ min/0
        , min/2
        , max/0
        , max/2
        , 'N'/0
        , 'N'/1
        , newtypeN_/0
        , stringAppend/0
        , stringAppend/1
        , inlineWrap/0
        , inlineVoid/0
        , inlineUnwrap/0
        , inlineUnsafeCoerce/0
        , inlineUnary/0
        , inlineUnary/3
        , inlineOver2/0
        , inlineOver/0
        , inlineOver/1
        , inlineListSingleton/0
        , inlineListCons/0
        , inlineListCons/2
        , inlineIntToNumber/0
        , inlineCoerce/0
        , inlineBinary/0
        , inlineBinary/5
        , inlineAtom/0
        , inlineOver2/2
        ]).
-compile(no_auto_import).
min() ->
  fun
    (X) ->
      fun
        (Y) ->
          min(X, Y)
      end
  end.

min(X, Y) ->
  begin
    V = ((erlang:map_get(compare, data_ord@ps:ordInt()))(X))(Y),
    case V of
      {lT} ->
        X;
      {eQ} ->
        X;
      {gT} ->
        Y;
      _ ->
        erlang:error({fail, <<"Failed pattern match">>})
    end
  end.

max() ->
  fun
    (X) ->
      fun
        (Y) ->
          max(X, Y)
      end
  end.

max(X, Y) ->
  begin
    V = ((erlang:map_get(compare, data_ord@ps:ordInt()))(X))(Y),
    case V of
      {lT} ->
        Y;
      {eQ} ->
        X;
      {gT} ->
        X;
      _ ->
        erlang:error({fail, <<"Failed pattern match">>})
    end
  end.

'N'() ->
  fun 'N'/1.

'N'(X) ->
  X.

newtypeN_() ->
  #{ 'Coercible0' =>
     fun
       (_) ->
         undefined
     end
   }.

stringAppend() ->
  fun stringAppend/1.

stringAppend(World) ->
  <<"Hello ", World/binary>>.

inlineWrap() ->
  1.

inlineVoid() ->
  fun
    () ->
      unit
  end.

inlineUnwrap() ->
  1.

inlineUnsafeCoerce() ->
  42.

inlineUnary() ->
  fun
    (I) ->
      fun
        (N) ->
          fun
            (B) ->
              inlineUnary(I, N, B)
          end
      end
  end.

inlineUnary(I, N, B) ->
  #{ negNum => - N, negInt => - I, notBoolean => not B }.

inlineOver2() ->
  data_semiring@ps:intAdd().

inlineOver() ->
  fun inlineOver/1.

inlineOver(V) ->
  V + 1.

inlineListSingleton() ->
  [1].

inlineListCons() ->
  fun
    (X) ->
      fun
        (V) ->
          inlineListCons(X, V)
      end
  end.

inlineListCons(X, V) ->
  [ X | V ].

inlineIntToNumber() ->
  data_int@foreign:toNumber(42).

inlineCoerce() ->
  42.

inlineBinary() ->
  fun
    (I) ->
      fun
        (N) ->
          fun
            (B) ->
              fun
                (S) ->
                  fun
                    (C) ->
                      inlineBinary(I, N, B, S, C)
                  end
              end
          end
      end
  end.

inlineBinary(I, N, B, S, C) ->
  #{ divInt => I div I
   , divNum => N / N
   , andBool => B andalso B
   , orBool => B orelse B
   , appendList =>
     fun
       (V) ->
         fun
           (L2) ->
             V ++ L2
         end
     end
   , addInt => I + I
   , mulInt => I * I
   , subInt => I - I
   , addNum => N + N
   , mulNum => N * N
   , subNum => N - N
   , eqNum => N =:= N
   , notEqNum => N =/= N
   , eqInt => I =:= I
   , notInt => I =/= I
   , eqString => S =:= S
   , notEqString => S =/= S
   , eqChar => C =:= C
   , notEqChar => C =/= C
   , eqBoolean => B =:= B
   , notEqBoolean => B =:= B
   , ltInt => I < I
   , lteInt => I =< I
   , gtInt => I > I
   , gteInt => I >= I
   , minInt => min(I, I)
   , maxInt => max(I, I)
   }.

inlineAtom() ->
  an_atom.

inlineOver2(V, V@1) ->
  V + V@1.


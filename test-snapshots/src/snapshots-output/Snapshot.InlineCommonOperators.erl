-module(snapshot_inlineCommonOperators@ps).
-export([min/0, min/2, max/0, max/2, 'N'/0, 'N'/1, newtypeN_/0, inlineWrap/0, inlineVoid/0, inlineUnwrap/0, inlineUnsafeCoerce/0, inlineUnary/0, inlineUnary/3, inlineOver2/0, inlineOver/0, inlineOver/1, inlineListSingleton/0, inlineListCons/0, inlineListCons/2, inlineIntToNumber/0, inlineCoerce/0, inlineBinary/0, inlineBinary/5, inlineAtom/0]).
min() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (min(V@0, V@1))
      end)
  end).
min(X, Y) ->
  begin
    V = (((maps:get(compare, (data_ord@ps:ordInt())))(X))(Y)),
    case (lT =:= (erlang:element(1, V))) of
      true ->
        X;
      _ ->
        case (eQ =:= (erlang:element(1, V))) of
          true ->
            X;
          _ ->
            case (gT =:= (erlang:element(1, V))) of
              true ->
                Y;
              _ ->
                (erlang:throw({fail,<<"Failed pattern match">>}))
            end
        end
    end
  end.
max() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (max(V@0, V@1))
      end)
  end).
max(X, Y) ->
  begin
    V = (((maps:get(compare, (data_ord@ps:ordInt())))(X))(Y)),
    case (lT =:= (erlang:element(1, V))) of
      true ->
        Y;
      _ ->
        case (eQ =:= (erlang:element(1, V))) of
          true ->
            X;
          _ ->
            case (gT =:= (erlang:element(1, V))) of
              true ->
                X;
              _ ->
                (erlang:throw({fail,<<"Failed pattern match">>}))
            end
        end
    end
  end.
'N'() ->
  (fun
    (V@0) ->
      ('N'(V@0))
  end).
'N'(X) ->
  X.
newtypeN_() ->
  #{'Coercible0' => (fun
    (_) ->
      undefined
  end)}.
inlineWrap() ->
  1.
inlineVoid() ->
  (fun
    () ->
      unit
  end).
inlineUnwrap() ->
  1.
inlineUnsafeCoerce() ->
  42.
inlineUnary() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (fun
            (V@2) ->
              (inlineUnary(V@0, V@1, V@2))
          end)
      end)
  end).
inlineUnary(I, N, B) ->
  #{negNum => (- N),
  negInt => (- I),
  notBoolean => (not B)}.
inlineOver2() ->
  (data_semiring@ps:intAdd()).
inlineOver() ->
  (fun
    (V@0) ->
      (inlineOver(V@0))
  end).
inlineOver(V) ->
  (V + 1).
inlineListSingleton() ->
  [1].
inlineListCons() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (inlineListCons(V@0, V@1))
      end)
  end).
inlineListCons(X, V) ->
  [X|V].
inlineIntToNumber() ->
  ((data_int@ps:toNumber())(42)).
inlineCoerce() ->
  42.
inlineBinary() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (fun
            (V@2) ->
              (fun
                (V@3) ->
                  (fun
                    (V@4) ->
                      (inlineBinary(V@0, V@1, V@2, V@3, V@4))
                  end)
              end)
          end)
      end)
  end).
inlineBinary(I, N, B, S, C) ->
  #{divInt => (I div I),
  divNum => (N / N),
  andBool => (B andalso B),
  orBool => (B orelse B),
  appendList => (fun
    (V) ->
      (fun
        (L2) ->
          (V ++ L2)
      end)
  end),
  addInt => (I + I),
  mulInt => (I * I),
  subInt => (I - I),
  addNum => (N + N),
  mulNum => (N * N),
  subNum => (N - N),
  eqNum => (N =:= N),
  notEqNum => (N =/= N),
  eqInt => (I =:= I),
  notInt => (I =/= I),
  eqString => (S =:= S),
  notEqString => (S =/= S),
  eqChar => (C =:= C),
  notEqChar => (C =/= C),
  eqBoolean => (B =:= B),
  notEqBoolean => (B =:= B),
  ltInt => (I < I),
  lteInt => (I =< I),
  gtInt => (I > I),
  gteInt => (I >= I),
  minInt => (((snapshot_inlineCommonOperators@ps:min())(I))(I)),
  maxInt => (((snapshot_inlineCommonOperators@ps:max())(I))(I))}.
inlineAtom() ->
  an_atom.

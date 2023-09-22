-module(snapshot_inlineCommonOperators@ps).
-export([min/0, max/0, 'N'/0, newtypeN_/0, inlineWrap/0, inlineVoid/0, inlineUnwrap/0, inlineUnsafeCoerce/0, inlineUnary/0, inlineOver2/0, inlineOver/0, inlineListSingleton/0, inlineListCons/0, inlineIntToNumber/0, inlineCoerce/0, inlineBinary/0, inlineAtom/0]).
min() ->
  (fun
    (X) ->
      (fun
        (Y) ->
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
          end
      end)
  end).
max() ->
  (fun
    (X) ->
      (fun
        (Y) ->
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
          end
      end)
  end).
'N'() ->
  (fun
    (X) ->
      X
  end).
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
    (I) ->
      (fun
        (N) ->
          (fun
            (B) ->
              #{negNum => (- N),
              negInt => (- I),
              notBoolean => (not B)}
          end)
      end)
  end).
inlineOver2() ->
  (data_semiring@ps:intAdd()).
inlineOver() ->
  (fun
    (V) ->
      (V + 1)
  end).
inlineListSingleton() ->
  [1].
inlineListCons() ->
  (fun
    (X) ->
      (fun
        (V) ->
          [X|V]
      end)
  end).
inlineIntToNumber() ->
  ((data_int@ps:toNumber())(42)).
inlineCoerce() ->
  42.
inlineBinary() ->
  (fun
    (I) ->
      (fun
        (N) ->
          (fun
            (B) ->
              (fun
                (S) ->
                  (fun
                    (C) ->
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
                      maxInt => (((snapshot_inlineCommonOperators@ps:max())(I))(I))}
                  end)
              end)
          end)
      end)
  end).
inlineAtom() ->
  an_atom.

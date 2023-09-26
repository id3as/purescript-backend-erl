-module(snapshot_inlineCommonOperators@ps).
-export([min/2, min/0, max/2, max/0, 'N'/1, 'N'/0, newtypeN_/0, inlineWrap/0, inlineVoid/0, inlineUnwrap/0, inlineUnsafeCoerce/0, inlineUnary/3, inlineUnary/0, inlineOver2/0, inlineOver/1, inlineOver/0, inlineListSingleton/0, inlineListCons/2, inlineListCons/0, inlineIntToNumber/0, inlineCoerce/0, inlineBinary/5, inlineBinary/0, inlineAtom/0]).
min(X, Y@1) ->
  begin
    V = (((maps:get(compare, (data_ord@ps:ordInt())))(X))(Y@1)),
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
                Y@1;
              _ ->
                (erlang:throw({fail,<<"Failed pattern match">>}))
            end
        end
    end
  end.
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
max(X, Y@1) ->
  begin
    V = (((maps:get(compare, (data_ord@ps:ordInt())))(X))(Y@1)),
    case (lT =:= (erlang:element(1, V))) of
      true ->
        Y@1;
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
'N'(X) ->
  X.
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
inlineUnary(I, N@1, B@2) ->
  #{negNum => (- N@1),
  negInt => (- I),
  notBoolean => (not B@2)}.
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
inlineOver(V) ->
  (V + 1).
inlineOver() ->
  (fun
    (V) ->
      (V + 1)
  end).
inlineListSingleton() ->
  [1].
inlineListCons(X, V@1) ->
  [X|V@1].
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
inlineBinary(I, N@1, B@2, S@3, C@4) ->
  #{divInt => (I div I),
  divNum => (N@1 / N@1),
  andBool => (B@2 andalso B@2),
  orBool => (B@2 orelse B@2),
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
  addNum => (N@1 + N@1),
  mulNum => (N@1 * N@1),
  subNum => (N@1 - N@1),
  eqNum => (N@1 =:= N@1),
  notEqNum => (N@1 =/= N@1),
  eqInt => (I =:= I),
  notInt => (I =/= I),
  eqString => (S@3 =:= S@3),
  notEqString => (S@3 =/= S@3),
  eqChar => (C@4 =:= C@4),
  notEqChar => (C@4 =/= C@4),
  eqBoolean => (B@2 =:= B@2),
  notEqBoolean => (B@2 =:= B@2),
  ltInt => (I < I),
  lteInt => (I =< I),
  gtInt => (I > I),
  gteInt => (I >= I),
  minInt => (((snapshot_inlineCommonOperators@ps:min())(I))(I)),
  maxInt => (((snapshot_inlineCommonOperators@ps:max())(I))(I))}.
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

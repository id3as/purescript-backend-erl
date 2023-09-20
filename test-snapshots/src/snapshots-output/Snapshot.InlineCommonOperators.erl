-module(snapshot_inlineCommonOperators@ps).
-export([min/0, max/0, 'N'/0, newtypeN_/0, inlineWrap/0, inlineVoid/0, inlineUnwrap/0, inlineUnsafeCoerce/0, inlineUnary/0, inlineOver2/0, inlineOver/0, inlineListSingleton/0, inlineListCons/0, inlineIntToNumber/0, inlineCoerce/0, inlineBinary/0, inlineAtom/0]).
min() ->
  (fun
    (X@0) ->
      (fun
        (Y@1) ->
          begin
            V@2 = (((maps:get(compare, (data_ord@ps:ordInt())))(X@0))(Y@1)),
            case (lT =:= (erlang:element(1, V@2))) of
              true ->
                X@0;
              _ ->
                case (eQ =:= (erlang:element(1, V@2))) of
                  true ->
                    X@0;
                  _ ->
                    case (gT =:= (erlang:element(1, V@2))) of
                      true ->
                        Y@1;
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
    (X@0) ->
      (fun
        (Y@1) ->
          begin
            V@2 = (((maps:get(compare, (data_ord@ps:ordInt())))(X@0))(Y@1)),
            case (lT =:= (erlang:element(1, V@2))) of
              true ->
                Y@1;
              _ ->
                case (eQ =:= (erlang:element(1, V@2))) of
                  true ->
                    X@0;
                  _ ->
                    case (gT =:= (erlang:element(1, V@2))) of
                      true ->
                        X@0;
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
    (X@0) ->
      X@0
  end).
newtypeN_() ->
  #{'Coercible0' => (fun
    (_@dollar__unused@0) ->
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
    (I@0) ->
      (fun
        (N@1) ->
          (fun
            (B@2) ->
              #{negNum => (- N@1),
              negInt => (- I@0),
              notBoolean => (not B@2)}
          end)
      end)
  end).
inlineOver2() ->
  (data_semiring@ps:intAdd()).
inlineOver() ->
  (fun
    (V@0) ->
      (V@0 + 1)
  end).
inlineListSingleton() ->
  [1].
inlineListCons() ->
  (fun
    (X@0) ->
      (fun
        (V@1) ->
          [X@0|V@1]
      end)
  end).
inlineIntToNumber() ->
  ((data_int@ps:toNumber())(42)).
inlineCoerce() ->
  42.
inlineBinary() ->
  (fun
    (I@0) ->
      (fun
        (N@1) ->
          (fun
            (B@2) ->
              (fun
                (S@3) ->
                  (fun
                    (C@4) ->
                      #{divInt => (I@0 div I@0),
                      divNum => (N@1 / N@1),
                      andBool => (B@2 andalso B@2),
                      orBool => (B@2 orelse B@2),
                      appendList => (fun
                        (V@5) ->
                          (fun
                            (L2@6) ->
                              (V@5 ++ L2@6)
                          end)
                      end),
                      addInt => (I@0 + I@0),
                      mulInt => (I@0 * I@0),
                      subInt => (I@0 - I@0),
                      addNum => (N@1 + N@1),
                      mulNum => (N@1 * N@1),
                      subNum => (N@1 - N@1),
                      eqNum => (N@1 =:= N@1),
                      notEqNum => (N@1 =/= N@1),
                      eqInt => (I@0 =:= I@0),
                      notInt => (I@0 =/= I@0),
                      eqString => (S@3 =:= S@3),
                      notEqString => (S@3 =/= S@3),
                      eqChar => (C@4 =:= C@4),
                      notEqChar => (C@4 =/= C@4),
                      eqBoolean => (B@2 =:= B@2),
                      notEqBoolean => (B@2 =:= B@2),
                      ltInt => (I@0 < I@0),
                      lteInt => (I@0 =< I@0),
                      gtInt => (I@0 > I@0),
                      gteInt => (I@0 >= I@0),
                      minInt => (((min())(I@0))(I@0)),
                      maxInt => (((max())(I@0))(I@0))}
                  end)
              end)
          end)
      end)
  end).
inlineAtom() ->
  an_atom.

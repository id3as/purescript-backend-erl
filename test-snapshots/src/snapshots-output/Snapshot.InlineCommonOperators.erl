-module(snapshot_inlineCommonOperators@ps).
-export([min/0, min/2, max/0, max/2, 'N'/0, 'N'/1, newtypeN_/0, inlineWrap/0, inlineVoid/0, inlineUnwrap/0, inlineUnsafeCoerce/0, inlineUnary/0, inlineUnary/3, inlineOver2/0, inlineOver/0, inlineOver/1, inlineListSingleton/0, inlineListCons/0, inlineListCons/2, inlineIntToNumber/0, inlineCoerce/0, inlineBinary/0, inlineBinary/5, inlineAtom/0]).
-compile(no_auto_import).
min() ->
  (fun
    (X@Local) ->
      (fun
        (Y@Local@1) ->
          (min(X@Local, Y@Local@1))
      end)
  end).
min(X, Y) ->
  begin
    V = (((maps:get(compare, (data_ord@ps:ordInt())))(X))(Y)),
    case ((erlang:is_tuple(V)) andalso ((1 =< (erlang:tuple_size(V))) andalso ((lT =:= (erlang:element(1, V))) andalso true))) of
      true ->
        X;
      _ ->
        case ((erlang:is_tuple(V)) andalso ((1 =< (erlang:tuple_size(V))) andalso ((eQ =:= (erlang:element(1, V))) andalso true))) of
          true ->
            X;
          _ ->
            case ((erlang:is_tuple(V)) andalso ((1 =< (erlang:tuple_size(V))) andalso ((gT =:= (erlang:element(1, V))) andalso true))) of
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
    (X@Local) ->
      (fun
        (Y@Local@1) ->
          (max(X@Local, Y@Local@1))
      end)
  end).
max(X, Y) ->
  begin
    V = (((maps:get(compare, (data_ord@ps:ordInt())))(X))(Y)),
    case ((erlang:is_tuple(V)) andalso ((1 =< (erlang:tuple_size(V))) andalso ((lT =:= (erlang:element(1, V))) andalso true))) of
      true ->
        Y;
      _ ->
        case ((erlang:is_tuple(V)) andalso ((1 =< (erlang:tuple_size(V))) andalso ((eQ =:= (erlang:element(1, V))) andalso true))) of
          true ->
            X;
          _ ->
            case ((erlang:is_tuple(V)) andalso ((1 =< (erlang:tuple_size(V))) andalso ((gT =:= (erlang:element(1, V))) andalso true))) of
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
    (X@Local) ->
      ('N'(X@Local))
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
    (I@Local) ->
      (fun
        (N@Local@1) ->
          (fun
            (B@Local@2) ->
              (inlineUnary(I@Local, N@Local@1, B@Local@2))
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
    (V@Local) ->
      (inlineOver(V@Local))
  end).
inlineOver(V) ->
  (V + 1).
inlineListSingleton() ->
  [1].
inlineListCons() ->
  (fun
    (X@Local) ->
      (fun
        (V@Local@1) ->
          (inlineListCons(X@Local, V@Local@1))
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
    (I@Local) ->
      (fun
        (N@Local@1) ->
          (fun
            (B@Local@2) ->
              (fun
                (S@Local@3) ->
                  (fun
                    (C@Local@4) ->
                      (inlineBinary(I@Local, N@Local@1, B@Local@2, S@Local@3, C@Local@4))
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

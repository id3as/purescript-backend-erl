-module(snapshot_prime@ps).
-export(['NCtor'/1, 'NCtor'/0, 'NewtypeCtor\''/1, 'NewtypeCtor\''/0, 'F1'/0, 'F2'/0, 'DCtor'/0, 'Ctor\''/0, useNewtypeType/1, useNewtypeType/0, useNewtypeCtor/1, useNewtypeCtor/0, useDataType/1, useDataType/0, useDataCtor/1, useDataCtor/0, normal/1, normal/0, useNormal/1, useNormal/0, 'instanceName\''/0, useNormal1/0, useInstance/0, ignore/1, ignore/0, useClass/1, useClass/0, 'foo\'oo'/0, useFooPrime3/0, 'foo\'\''/0, useFooPrime2/0, 'foo\''/0, useFooPrime1/0, result/0, foo/0, 'classMember\''/1, 'classMember\''/0, useMember/1, useMember/0]).
'NCtor'(X) ->
  X.
'NCtor'() ->
  (fun
    (X) ->
      X
  end).
'NewtypeCtor\''(X) ->
  X.
'NewtypeCtor\''() ->
  (fun
    (X) ->
      X
  end).
'F1'() ->
  {f1}.
'F2'() ->
  {f2}.
'DCtor'() ->
  {dCtor}.
'Ctor\''() ->
  (fun
    (Value0) ->
      (fun
        (Value1) ->
          {'ctor\'',Value0,Value1}
      end)
  end).
useNewtypeType(I) ->
  I.
useNewtypeType() ->
  (fun
    (I) ->
      I
  end).
useNewtypeCtor(I) ->
  I.
useNewtypeCtor() ->
  (fun
    (I) ->
      I
  end).
useDataType(V) ->
  {dCtor}.
useDataType() ->
  (fun
    (_) ->
      {dCtor}
  end).
useDataCtor(S) ->
  {'ctor\'',S,4}.
useDataCtor() ->
  (fun
    (S) ->
      {'ctor\'',S,4}
  end).
normal(Dict) ->
  (maps:get(normal, Dict)).
normal() ->
  (fun
    (Dict) ->
      (maps:get(normal, Dict))
  end).
useNormal(DictNormal) ->
  begin
    Normal1 = ((snapshot_prime@ps:normal())(DictNormal)),
    (fun
      (DictNormal1) ->
        begin
          Normal2 = ((snapshot_prime@ps:normal())(DictNormal1)),
          (fun
            (A) ->
              (fun
                (B) ->
                  <<(Normal1(A))/binary, (Normal2(B))/binary>>
              end)
          end)
        end
    end)
  end.
useNormal() ->
  (fun
    (DictNormal) ->
      begin
        Normal1 = ((snapshot_prime@ps:normal())(DictNormal)),
        (fun
          (DictNormal1) ->
            begin
              Normal2 = ((snapshot_prime@ps:normal())(DictNormal1)),
              (fun
                (A) ->
                  (fun
                    (B) ->
                      <<(Normal1(A))/binary, (Normal2(B))/binary>>
                  end)
              end)
            end
        end)
      end
  end).
'instanceName\''() ->
  #{normal => (fun
    (V) ->
      case (f1 =:= (erlang:element(1, V))) of
        true ->
          <<"F1">>;
        _ ->
          case (f2 =:= (erlang:element(1, V))) of
            true ->
              <<"F2">>;
            _ ->
              (erlang:throw({fail,<<"Failed pattern match">>}))
          end
      end
  end)}.
useNormal1() ->
  (((snapshot_prime@ps:useNormal())((snapshot_prime@ps:'instanceName\''())))((snapshot_prime@ps:'instanceName\''()))).
useInstance() ->
  (((snapshot_prime@ps:useNormal1())({f1}))({f2})).
ignore(Dict) ->
  (maps:get(ignore, Dict)).
ignore() ->
  (fun
    (Dict) ->
      (maps:get(ignore, Dict))
  end).
useClass(DictClassName_@prime) ->
  ((snapshot_prime@ps:ignore())(DictClassName_@prime)).
useClass() ->
  (fun
    (DictClassName_@prime) ->
      ((snapshot_prime@ps:ignore())(DictClassName_@prime))
  end).
'foo\'oo'() ->
  <<"foo\'oo">>.
useFooPrime3() ->
  (snapshot_prime@ps:'foo\'oo'()).
'foo\'\''() ->
  <<"foo\'\'">>.
useFooPrime2() ->
  (snapshot_prime@ps:'foo\'\''()).
'foo\''() ->
  <<"foo\'">>.
useFooPrime1() ->
  (snapshot_prime@ps:'foo\''()).
result() ->
  #{test1 => ((snapshot_prime@ps:'foo\''()) =:= (snapshot_prime@ps:'foo\''())),
  test2 => ((snapshot_prime@ps:'foo\'\''()) =:= (snapshot_prime@ps:'foo\'\''())),
  test3 => ((snapshot_prime@ps:'foo\'oo'()) =:= (snapshot_prime@ps:'foo\'oo'())),
  useInstance => (((snapshot_prime@ps:useNormal1())({f1}))({f2}))}.
foo() ->
  <<"foo">>.
'classMember\''(Dict) ->
  (maps:get('classMember\'', Dict)).
'classMember\''() ->
  (fun
    (Dict) ->
      (maps:get('classMember\'', Dict))
  end).
useMember(DictClassMember) ->
  ((snapshot_prime@ps:'classMember\''())(DictClassMember)).
useMember() ->
  (fun
    (DictClassMember) ->
      ((snapshot_prime@ps:'classMember\''())(DictClassMember))
  end).

-module(snapshot_prime@ps).
-export(['NCtor'/0, 'NCtor'/1, 'NewtypeCtor\''/0, 'NewtypeCtor\''/1, 'F1'/0, 'F2'/0, 'DCtor'/0, 'Ctor\''/0, useNewtypeType/0, useNewtypeType/1, useNewtypeCtor/0, useNewtypeCtor/1, useDataType/0, useDataType/1, useDataCtor/0, useDataCtor/1, normal/0, normal/1, useNormal/0, useNormal/1, 'instanceName\''/0, useNormal1/0, useInstance/0, ignore/0, ignore/1, useClass/0, useClass/1, 'foo\'oo'/0, useFooPrime3/0, 'foo\'\''/0, useFooPrime2/0, 'foo\''/0, useFooPrime1/0, result/0, foo/0, 'classMember\''/0, 'classMember\''/1, useMember/0, useMember/1]).
'NCtor'() ->
  (fun
    (V@0) ->
      ('NCtor'(V@0))
  end).
'NCtor'(X) ->
  X.
'NewtypeCtor\''() ->
  (fun
    (V@0) ->
      ('NewtypeCtor\''(V@0))
  end).
'NewtypeCtor\''(X) ->
  X.
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
useNewtypeType() ->
  (fun
    (V@0) ->
      (useNewtypeType(V@0))
  end).
useNewtypeType(I) ->
  I.
useNewtypeCtor() ->
  (fun
    (V@0) ->
      (useNewtypeCtor(V@0))
  end).
useNewtypeCtor(I) ->
  I.
useDataType() ->
  (fun
    (V@0) ->
      (useDataType(V@0))
  end).
useDataType(_) ->
  {dCtor}.
useDataCtor() ->
  (fun
    (V@0) ->
      (useDataCtor(V@0))
  end).
useDataCtor(S) ->
  {'ctor\'',S,4}.
normal() ->
  (fun
    (V@0) ->
      (normal(V@0))
  end).
normal(Dict) ->
  (maps:get(normal, Dict)).
useNormal() ->
  (fun
    (V@0) ->
      (useNormal(V@0))
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
ignore() ->
  (fun
    (V@0) ->
      (ignore(V@0))
  end).
ignore(Dict) ->
  (maps:get(ignore, Dict)).
useClass() ->
  (fun
    (V@0) ->
      (useClass(V@0))
  end).
useClass(DictClassName_@prime) ->
  ((snapshot_prime@ps:ignore())(DictClassName_@prime)).
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
'classMember\''() ->
  (fun
    (V@0) ->
      ('classMember\''(V@0))
  end).
'classMember\''(Dict) ->
  (maps:get('classMember\'', Dict)).
useMember() ->
  (fun
    (V@0) ->
      (useMember(V@0))
  end).
useMember(DictClassMember) ->
  ((snapshot_prime@ps:'classMember\''())(DictClassMember)).

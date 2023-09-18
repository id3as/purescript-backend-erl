-module(snapshot_prime@ps).
-export(['NCtor'/0, 'NewtypeCtor\''/0, 'F1'/0, 'F2'/0, 'DCtor'/0, 'Ctor\''/0, useNewtypeType/0, useNewtypeCtor/0, useDataType/0, useDataCtor/0, normal/0, useNormal/0, 'instanceName\''/0, useNormal1/0, useInstance/0, ignore/0, useClass/0, 'foo\'oo'/0, useFooPrime3/0, 'foo\'\''/0, useFooPrime2/0, 'foo\''/0, useFooPrime1/0, result/0, foo/0, 'classMember\''/0, useMember/0]).
'NCtor'() ->
  (fun
    (X@0) ->
      X@0
  end).
'NewtypeCtor\''() ->
  (fun
    (X@0) ->
      X@0
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
useNewtypeType() ->
  (fun
    (I@0) ->
      I@0
  end).
useNewtypeCtor() ->
  (fun
    (I@0) ->
      I@0
  end).
useDataType() ->
  (fun
    (V@0) ->
      ('DCtor'())
  end).
useDataCtor() ->
  (fun
    (S@0) ->
      {'ctor\'',S@0,4}
  end).
normal() ->
  (fun
    (Dict@0) ->
      (maps:get(normal, Dict@0))
  end).
useNormal() ->
  (fun
    (DictNormal@0) ->
      begin
        Normal1@1 = ((normal())(DictNormal@0)),
        (fun
          (DictNormal1@2) ->
            begin
              Normal2@3 = ((normal())(DictNormal1@2)),
              (fun
                (A@4) ->
                  (fun
                    (B@5) ->
                      (unicode:characters_to_binary([(Normal1@1(A@4)),(Normal2@3(B@5))], utf8))
                  end)
              end)
            end
        end)
      end
  end).
'instanceName\''() ->
  #{normal => (fun
    (V@0) ->
      case (f1 =:= (erlang:element(1, V@0))) of
        true ->
          <<"F1">>;
        _ ->
          case (f2 =:= (erlang:element(1, V@0))) of
            true ->
              <<"F2">>;
            _ ->
              (erlang:throw({fail,<<"Failed pattern match">>}))
          end
      end
  end)}.
useNormal1() ->
  (((useNormal())(('instanceName\''())))(('instanceName\''()))).
useInstance() ->
  (((useNormal1())(('F1'())))(('F2'()))).
ignore() ->
  (fun
    (Dict@0) ->
      (maps:get(ignore, Dict@0))
  end).
useClass() ->
  (fun
    (DictClassName_@prime@0) ->
      ((ignore())(DictClassName_@prime@0))
  end).
'foo\'oo'() ->
  <<"foo\'oo">>.
useFooPrime3() ->
  ('foo\'oo'()).
'foo\'\''() ->
  <<"foo\'\'">>.
useFooPrime2() ->
  ('foo\'\''()).
'foo\''() ->
  <<"foo\'">>.
useFooPrime1() ->
  ('foo\''()).
result() ->
  #{test1 => (('foo\''()) =:= ('foo\''())),
  test2 => (('foo\'\''()) =:= ('foo\'\''())),
  test3 => (('foo\'oo'()) =:= ('foo\'oo'())),
  useInstance => (((useNormal1())(('F1'())))(('F2'())))}.
foo() ->
  <<"foo">>.
'classMember\''() ->
  (fun
    (Dict@0) ->
      (maps:get('classMember\'', Dict@0))
  end).
useMember() ->
  (fun
    (DictClassMember@0) ->
      (('classMember\''())(DictClassMember@0))
  end).

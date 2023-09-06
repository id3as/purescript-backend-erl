-module(snapshot_prime).
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
?
?
?
?
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
      (snapshot_prime:'DCtor'())
  end).
useDataCtor() -> 
  (fun
    (S@0) -> 
      ?ctor_saturated
  end).
normal() -> 
  (fun
    (Dict@0) -> 
      (maps:get(normal, Dict@0))
  end).
useNormal() -> 
  (fun
    (DictNormal@0) -> 
      (fun
        (DictNormal1@1) -> 
          (fun
            (A@2) -> 
              (fun
                (B@3) -> 
                  (unicode:characters_to_binary([((maps:get(normal, DictNormal@0))(A@2)),((maps:get(normal, DictNormal1@1))(B@3))], utf8))
              end)
          end)
      end)
  end).
'instanceName\''() -> 
  #{normal => (fun
    (V@0) -> 
      case ?istag of
        true ->
          <<"F1">>;
        _ ->
          case ?istag of
            true ->
              <<"F2">>;
            _ ->
              ?fail
          end
      end
  end)}.
useInstance() -> 
  <<"F1F2">>.
ignore() -> 
  (fun
    (Dict@0) -> 
      (maps:get(ignore, Dict@0))
  end).
useClass() -> 
  (fun
    (DictClassName'@0) -> 
      (maps:get(ignore, DictClassName'@0))
  end).
'foo\'oo'() -> 
  <<"foo'oo">>.
useFooPrime3() -> 
  <<"foo'oo">>.
'foo\'\''() -> 
  <<"foo'">>.
useFooPrime2() -> 
  <<"foo'">>.
'foo\''() -> 
  <<"foo'">>.
useFooPrime1() -> 
  <<"foo'">>.
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
      (maps:get('classMember\'', DictClassMember@0))
  end).

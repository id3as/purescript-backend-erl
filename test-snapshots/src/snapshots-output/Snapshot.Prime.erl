-module('Snapshot.Prime').
-compile(export_all).
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
      ('Snapshot.Prime':'DCtor'())
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
      case (erlang:element(1, V@0)) of
        true ->
          <<"F1"/utf8>>;
        _ ->
          case (erlang:element(1, V@0)) of
            true ->
              <<"F2"/utf8>>;
            _ ->
              (erlang:throw({fail,<<"Failed pattern match"/utf8>>}))
          end
      end
  end)}.
useInstance() -> 
  <<"F1F2"/utf8>>.
ignore() -> 
  (fun
    (Dict@0) ->
      (maps:get(ignore, Dict@0))
  end).
useClass() -> 
  (fun
    (DictClassName_@prime@0) ->
      (maps:get(ignore, DictClassName_@prime@0))
  end).
'foo\'oo'() -> 
  <<"foo\'oo"/utf8>>.
useFooPrime3() -> 
  <<"foo\'oo"/utf8>>.
'foo\'\''() -> 
  <<"foo\'"/utf8>>.
useFooPrime2() -> 
  <<"foo\'"/utf8>>.
'foo\''() -> 
  <<"foo\'"/utf8>>.
useFooPrime1() -> 
  <<"foo\'"/utf8>>.
foo() -> 
  <<"foo"/utf8>>.
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

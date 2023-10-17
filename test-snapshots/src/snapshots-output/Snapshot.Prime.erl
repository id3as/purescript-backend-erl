-module(snapshot_prime@ps).
-export([ 'NCtor'/0
        , 'NCtor'/1
        , 'NewtypeCtor\''/0
        , 'NewtypeCtor\''/1
        , 'F1'/0
        , 'F2'/0
        , 'DCtor'/0
        , 'Ctor\''/0
        , useNewtypeType/0
        , useNewtypeType/1
        , useNewtypeCtor/0
        , useNewtypeCtor/1
        , useDataType/0
        , useDataType/1
        , useDataCtor/0
        , useDataCtor/1
        , normal/0
        , normal/1
        , useNormal/0
        , useNormal/1
        , 'instanceName\''/0
        , useNormal1/0
        , useInstance/0
        , ignore/0
        , ignore/1
        , useClass/0
        , useClass/1
        , 'foo\'oo'/0
        , useFooPrime3/0
        , 'foo\'\''/0
        , useFooPrime2/0
        , 'foo\''/0
        , useFooPrime1/0
        , result/0
        , foo/0
        , 'classMember\''/0
        , 'classMember\''/1
        , useMember/0
        , useMember/1
        ]).
-compile(no_auto_import).
-define( IS_KNOWN_TAG(Tag, Arity, V)
       , ((erlang:is_tuple(V))
         andalso ((1 =< (erlang:tuple_size(V)))
           andalso (Tag =:= (erlang:element(1, V)))))
       ).
'NCtor'() ->
  fun
    (X) ->
      'NCtor'(X)
  end.

'NCtor'(X) ->
  X.

'NewtypeCtor\''() ->
  fun
    (X) ->
      'NewtypeCtor\''(X)
  end.

'NewtypeCtor\''(X) ->
  X.

'F1'() ->
  {f1}.

'F2'() ->
  {f2}.

'DCtor'() ->
  {dCtor}.

'Ctor\''() ->
  fun
    (Value0) ->
      fun
        (Value1) ->
          {'ctor\'', Value0, Value1}
      end
  end.

useNewtypeType() ->
  fun
    (I) ->
      useNewtypeType(I)
  end.

useNewtypeType(I) ->
  I.

useNewtypeCtor() ->
  fun
    (I) ->
      useNewtypeCtor(I)
  end.

useNewtypeCtor(I) ->
  I.

useDataType() ->
  fun
    (V) ->
      useDataType(V)
  end.

useDataType(_) ->
  {dCtor}.

useDataCtor() ->
  fun
    (S) ->
      useDataCtor(S)
  end.

useDataCtor(S) ->
  {'ctor\'', S, 4}.

normal() ->
  fun
    (Dict) ->
      normal(Dict)
  end.

normal(#{ normal := Dict@1 }) ->
  Dict@1.

useNormal() ->
  fun
    (DictNormal) ->
      useNormal(DictNormal)
  end.

useNormal(DictNormal) ->
  begin
    Normal1 = normal(DictNormal),
    fun
      (DictNormal1) ->
        begin
          Normal2 = normal(DictNormal1),
          fun
            (A) ->
              fun
                (B) ->
                  <<(Normal1(A))/binary, (Normal2(B))/binary>>
              end
          end
        end
    end
  end.

'instanceName\''() ->
  #{ normal =>
     fun
       (V) ->
         if
           ?IS_KNOWN_TAG(f1, 0, V) ->
             <<"F1">>;
           ?IS_KNOWN_TAG(f2, 0, V) ->
             <<"F2">>;
           true ->
             erlang:throw({fail, <<"Failed pattern match">>})
         end
     end
   }.

useNormal1() ->
  (useNormal('instanceName\''()))('instanceName\''()).

useInstance() ->
  ((useNormal1())({f1}))({f2}).

ignore() ->
  fun
    (Dict) ->
      ignore(Dict)
  end.

ignore(#{ ignore := Dict@1 }) ->
  Dict@1.

useClass() ->
  fun
    (DictClassName_) ->
      useClass(DictClassName_)
  end.

useClass(DictClassName_) ->
  ignore(DictClassName_).

'foo\'oo'() ->
  <<"foo\'oo">>.

useFooPrime3() ->
  'foo\'oo'().

'foo\'\''() ->
  <<"foo\'\'">>.

useFooPrime2() ->
  'foo\'\''().

'foo\''() ->
  <<"foo\'">>.

useFooPrime1() ->
  'foo\''().

result() ->
  #{ test1 => ('foo\''()) =:= ('foo\''())
   , test2 => ('foo\'\''()) =:= ('foo\'\''())
   , test3 => ('foo\'oo'()) =:= ('foo\'oo'())
   , useInstance => ((useNormal1())({f1}))({f2})
   }.

foo() ->
  <<"foo">>.

'classMember\''() ->
  fun
    (Dict) ->
      'classMember\''(Dict)
  end.

'classMember\''(#{ 'classMember\'' := Dict@1 }) ->
  Dict@1.

useMember() ->
  fun
    (DictClassMember) ->
      useMember(DictClassMember)
  end.

useMember(DictClassMember) ->
  'classMember\''(DictClassMember).


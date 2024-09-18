% Snapshot.Prime
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
-define( MEMOIZE_AS(Key, _Metadata, Expr)
       , case persistent_term:get(Key, undefined) of
           undefined ->
             begin
               MemoizeAsResult = Expr,
               persistent_term:put(Key, MemoizeAsResult),
               MemoizeAsResult
             end;
           MemoizeAsResult ->
             MemoizeAsResult
         end
       ).

'NCtor'() ->
  fun 'NCtor'/1.

'NCtor'(X) ->
  X.

'NewtypeCtor\''() ->
  fun 'NewtypeCtor\''/1.

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
  fun useNewtypeType/1.

useNewtypeType(I) ->
  I.

useNewtypeCtor() ->
  fun useNewtypeCtor/1.

useNewtypeCtor(I) ->
  I.

useDataType() ->
  fun useDataType/1.

useDataType(_) ->
  {dCtor}.

useDataCtor() ->
  fun useDataCtor/1.

useDataCtor(S) ->
  {'ctor\'', S, 4}.

normal() ->
  fun normal/1.

normal(#{ normal := Dict }) ->
  Dict.

useNormal() ->
  fun useNormal/1.

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
         case V of
           {f1} ->
             <<"F1">>;
           {f2} ->
             <<"F2">>;
           _ ->
             erlang:error({fail, <<"Failed pattern match">>})
         end
     end
   }.

useNormal1() ->
  begin
    V = 'instanceName\''(),
    (useNormal(V))(V)
  end.

useInstance() ->
  ((useNormal1())({f1}))({f2}).

ignore() ->
  fun ignore/1.

ignore(#{ ignore := Dict }) ->
  Dict.

useClass() ->
  fun useClass/1.

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
  ?MEMOIZE_AS(
    {snapshot_prime@ps, result, '(memoized)'},
    29,
    begin
      V = 'foo\'oo'(),
      V@1 = 'foo\'\''(),
      V@2 = 'foo\''(),
      #{ test1 => V@2 =:= V@2
       , test2 => V@1 =:= V@1
       , test3 => V =:= V
       , useInstance => ((useNormal1())({f1}))({f2})
       }
    end
  ).

foo() ->
  <<"foo">>.

'classMember\''() ->
  fun 'classMember\''/1.

'classMember\''(#{ 'classMember\'' := Dict }) ->
  Dict.

useMember() ->
  fun useMember/1.

useMember(DictClassMember) ->
  'classMember\''(DictClassMember).


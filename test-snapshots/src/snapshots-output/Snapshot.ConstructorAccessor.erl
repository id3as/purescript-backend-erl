% Snapshot.ConstructorAccessor
-module(snapshot_constructorAccessor@ps).
-export([ 'First'/0
        , 'Last'/0
        , 'NoArgs'/0
        , 'HasArgs'/0
        , 'Foo'/0
        , z/0
        , y/0
        , x/0
        , test5/0
        , test5/2
        , test51/0
        , test51/1
        , test4/0
        , test4/1
        , test3/0
        , test3/1
        , test2/0
        , test2/1
        , test1/0
        , test1/1
        , 'don\'tInlineMeMe'/0
        , 'don\'tInlineMeMe'/1
        , result/0
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

'First'() ->
  fun
    (Value0) ->
      {first, Value0}
  end.

'Last'() ->
  fun
    (Value0) ->
      {last, Value0}
  end.

'NoArgs'() ->
  {noArgs}.

'HasArgs'() ->
  fun
    (Value0) ->
      fun
        (Value1) ->
          fun
            (Value2) ->
              {hasArgs, Value0, Value1, Value2}
          end
      end
  end.

'Foo'() ->
  fun
    (Value0) ->
      fun
        (Value1) ->
          fun
            (Value2) ->
              {foo, Value0, Value1, Value2}
          end
      end
  end.

z() ->
  {foo, 1, 1, 1}.

y() ->
  (('Foo'())(1))(1).

x() ->
  ('Foo'())(1).

test5() ->
  fun
    (V) ->
      fun
        (V@1) ->
          test5(V, V@1)
      end
  end.

test5(_, V = {first, V@1}) ->
  case V of
    {first, _} ->
      V@1;
    _ ->
      erlang:error({fail, <<"Failed pattern match">>})
  end.

test51() ->
  fun test51/1.

test51(V = {first, V@1}) ->
  case V of
    {first, _} ->
      V@1;
    _ ->
      erlang:error({fail, <<"Failed pattern match">>})
  end.

test4() ->
  fun test4/1.

test4(V) ->
  case V of
    {first, V@1} ->
      V@1;
    {last, V@2} ->
      V@2;
    _ ->
      erlang:error({fail, <<"Failed pattern match">>})
  end.

test3() ->
  fun test3/1.

test3(V) ->
  if
    (erlang:element(2, V)) < (erlang:element(4, V)) ->
      erlang:element(2, V);
    true ->
      erlang:element(3, V)
  end.

test2() ->
  fun test2/1.

test2(V) ->
  erlang:element(2, V).

test1() ->
  fun test1/1.

test1(_) ->
  true.

'don\'tInlineMeMe'() ->
  fun 'don\'tInlineMeMe'/1.

'don\'tInlineMeMe'(A) ->
  A.

result() ->
  ?MEMOIZE_AS(
    {snapshot_constructorAccessor@ps, result, '(memoized)'},
    31,
    #{ test1 => ('don\'tInlineMeMe'(fun test1/1))({noArgs})
     , test2 => ('don\'tInlineMeMe'(fun test2/1))({hasArgs, 2, 1, 0})
     , test3 => ('don\'tInlineMeMe'(fun test3/1))({hasArgs, 5, 3, 1})
     , test4 => ('don\'tInlineMeMe'(fun test4/1))({last, 4})
     , test5 => ('don\'tInlineMeMe'(fun test51/1))({first, 5})
     }
  ).


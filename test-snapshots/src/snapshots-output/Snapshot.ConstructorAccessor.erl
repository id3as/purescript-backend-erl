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
-define( IS_KNOWN_TAG(Tag, Arity, V)
       , ((erlang:is_tuple(V))
         andalso ((1 =< (erlang:tuple_size(V)))
           andalso (Tag =:= (erlang:element(1, V)))))
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
    (_@dollar__unused@Local) ->
      fun
        (V@Local@1) ->
          test5(_@dollar__unused@Local, V@Local@1)
      end
  end.

test5(_, V) ->
  if
    ?IS_KNOWN_TAG(first, 1, V) ->
      erlang:element(2, V);
    true ->
      erlang:throw({fail, <<"Failed pattern match">>})
  end.

test51() ->
  fun
    (V@Local) ->
      test51(V@Local)
  end.

test51(V) ->
  if
    ?IS_KNOWN_TAG(first, 1, V) ->
      erlang:element(2, V);
    true ->
      erlang:throw({fail, <<"Failed pattern match">>})
  end.

test4() ->
  fun
    (V@Local) ->
      test4(V@Local)
  end.

test4(V) ->
  if
    ?IS_KNOWN_TAG(first, 1, V) ->
      erlang:element(2, V);
    ?IS_KNOWN_TAG(last, 1, V) ->
      erlang:element(2, V);
    true ->
      erlang:throw({fail, <<"Failed pattern match">>})
  end.

test3() ->
  fun
    (V@Local) ->
      test3(V@Local)
  end.

test3(V) ->
  if
    (erlang:element(2, V)) < (erlang:element(4, V)) ->
      erlang:element(2, V);
    true ->
      erlang:element(3, V)
  end.

test2() ->
  fun
    (V@Local) ->
      test2(V@Local)
  end.

test2(V) ->
  erlang:element(2, V).

test1() ->
  fun
    (V@Local) ->
      test1(V@Local)
  end.

test1(_) ->
  true.

'don\'tInlineMeMe'() ->
  fun
    (A@Local) ->
      'don\'tInlineMeMe'(A@Local)
  end.

'don\'tInlineMeMe'(A) ->
  A.

result() ->
  #{ test1 => ('don\'tInlineMeMe'(test1()))({noArgs})
   , test2 => ('don\'tInlineMeMe'(test2()))({hasArgs, 2, 1, 0})
   , test3 => ('don\'tInlineMeMe'(test3()))({hasArgs, 5, 3, 1})
   , test4 => ('don\'tInlineMeMe'(test4()))({last, 4})
   , test5 => ('don\'tInlineMeMe'(test51()))({first, 5})
   }.


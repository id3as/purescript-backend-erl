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
-define( IS_TAG(Tag, V)
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
  ((snapshot_constructorAccessor@ps:'Foo'())(1))(1).

x() ->
  (snapshot_constructorAccessor@ps:'Foo'())(1).

test5() ->
  fun
    (_@dollar__unused@Local) ->
      fun
        (V@Local@1) ->
          test5(_@dollar__unused@Local, V@Local@1)
      end
  end.

test5(_, V) ->
  case ?IS_TAG(first, V) of
    true ->
      erlang:element(2, V);
    _ ->
      erlang:throw({fail, <<"Failed pattern match">>})
  end.

test51() ->
  fun
    (V@Local) ->
      test51(V@Local)
  end.

test51(V) ->
  case ?IS_TAG(first, V) of
    true ->
      erlang:element(2, V);
    _ ->
      erlang:throw({fail, <<"Failed pattern match">>})
  end.

test4() ->
  fun
    (V@Local) ->
      test4(V@Local)
  end.

test4(V) ->
  case ?IS_TAG(first, V) of
    true ->
      erlang:element(2, V);
    _ ->
      case ?IS_TAG(last, V) of
        true ->
          erlang:element(2, V);
        _ ->
          erlang:throw({fail, <<"Failed pattern match">>})
      end
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
  #{ test1 => ((snapshot_constructorAccessor@ps:'don\'tInlineMeMe'())
               (snapshot_constructorAccessor@ps:test1()))
              ({noArgs})
   , test2 => ((snapshot_constructorAccessor@ps:'don\'tInlineMeMe'())
               (snapshot_constructorAccessor@ps:test2()))
              ({hasArgs, 2, 1, 0})
   , test3 => ((snapshot_constructorAccessor@ps:'don\'tInlineMeMe'())
               (snapshot_constructorAccessor@ps:test3()))
              ({hasArgs, 5, 3, 1})
   , test4 => ((snapshot_constructorAccessor@ps:'don\'tInlineMeMe'())
               (snapshot_constructorAccessor@ps:test4()))
              ({last, 4})
   , test5 => ((snapshot_constructorAccessor@ps:'don\'tInlineMeMe'())
               (snapshot_constructorAccessor@ps:test51()))
              ({first, 5})
   }.


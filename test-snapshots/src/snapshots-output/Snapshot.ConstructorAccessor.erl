-module(snapshot_constructorAccessor@ps).
-export(['First'/0, 'Last'/0, 'NoArgs'/0, 'HasArgs'/0, 'Foo'/0, z/0, y/0, x/0, test5/0, test5/2, test51/0, test51/1, test4/0, test4/1, test3/0, test3/1, test2/0, test2/1, test1/0, test1/1, 'don\'tInlineMeMe'/0, 'don\'tInlineMeMe'/1, result/0]).
'First'() ->
  (fun
    (Value0) ->
      {first,Value0}
  end).
'Last'() ->
  (fun
    (Value0) ->
      {last,Value0}
  end).
'NoArgs'() ->
  {noArgs}.
'HasArgs'() ->
  (fun
    (Value0) ->
      (fun
        (Value1) ->
          (fun
            (Value2) ->
              {hasArgs,Value0,Value1,Value2}
          end)
      end)
  end).
'Foo'() ->
  (fun
    (Value0) ->
      (fun
        (Value1) ->
          (fun
            (Value2) ->
              {foo,Value0,Value1,Value2}
          end)
      end)
  end).
z() ->
  {foo,1,1,1}.
y() ->
  (((snapshot_constructorAccessor@ps:'Foo'())(1))(1)).
x() ->
  ((snapshot_constructorAccessor@ps:'Foo'())(1)).
test5() ->
  (fun
    (V@0) ->
      (fun
        (V@1) ->
          (test5(V@0, V@1))
      end)
  end).
test5(_, V) ->
  case (first =:= (erlang:element(1, V))) of
    true ->
      (erlang:element(2, V));
    _ ->
      (erlang:throw({fail,<<"Failed pattern match">>}))
  end.
test51() ->
  (fun
    (V@0) ->
      (test51(V@0))
  end).
test51(V) ->
  case (first =:= (erlang:element(1, V))) of
    true ->
      (erlang:element(2, V));
    _ ->
      (erlang:throw({fail,<<"Failed pattern match">>}))
  end.
test4() ->
  (fun
    (V@0) ->
      (test4(V@0))
  end).
test4(V) ->
  case (first =:= (erlang:element(1, V))) of
    true ->
      (erlang:element(2, V));
    _ ->
      case (last =:= (erlang:element(1, V))) of
        true ->
          (erlang:element(2, V));
        _ ->
          (erlang:throw({fail,<<"Failed pattern match">>}))
      end
  end.
test3() ->
  (fun
    (V@0) ->
      (test3(V@0))
  end).
test3(V) ->
  case ((erlang:element(2, V)) < (erlang:element(4, V))) of
    true ->
      (erlang:element(2, V));
    _ ->
      (erlang:element(3, V))
  end.
test2() ->
  (fun
    (V@0) ->
      (test2(V@0))
  end).
test2(V) ->
  (erlang:element(2, V)).
test1() ->
  (fun
    (V@0) ->
      (test1(V@0))
  end).
test1(_) ->
  true.
'don\'tInlineMeMe'() ->
  (fun
    (V@0) ->
      ('don\'tInlineMeMe'(V@0))
  end).
'don\'tInlineMeMe'(A) ->
  A.
result() ->
  #{test1 => (((snapshot_constructorAccessor@ps:'don\'tInlineMeMe'())((snapshot_constructorAccessor@ps:test1())))({noArgs})),
  test2 => (((snapshot_constructorAccessor@ps:'don\'tInlineMeMe'())((snapshot_constructorAccessor@ps:test2())))({hasArgs,2,1,0})),
  test3 => (((snapshot_constructorAccessor@ps:'don\'tInlineMeMe'())((snapshot_constructorAccessor@ps:test3())))({hasArgs,5,3,1})),
  test4 => (((snapshot_constructorAccessor@ps:'don\'tInlineMeMe'())((snapshot_constructorAccessor@ps:test4())))({last,4})),
  test5 => (((snapshot_constructorAccessor@ps:'don\'tInlineMeMe'())((snapshot_constructorAccessor@ps:test51())))({first,5}))}.

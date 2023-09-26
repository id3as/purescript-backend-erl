-module(snapshot_constructorAccessor@ps).
-export(['First'/0, 'Last'/0, 'NoArgs'/0, 'HasArgs'/0, 'Foo'/0, z/0, y/0, x/0, test5/2, test5/0, test51/1, test51/0, test4/1, test4/0, test3/1, test3/0, test2/1, test2/0, test1/1, test1/0, 'don\'tInlineMeMe'/1, 'don\'tInlineMeMe'/0, result/0]).
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
test5(_@dollar__unused, V@1) ->
  case (first =:= (erlang:element(1, V@1))) of
    true ->
      (erlang:element(2, V@1));
    _ ->
      (erlang:throw({fail,<<"Failed pattern match">>}))
  end.
test5() ->
  (fun
    (_) ->
      (fun
        (V) ->
          case (first =:= (erlang:element(1, V))) of
            true ->
              (erlang:element(2, V));
            _ ->
              (erlang:throw({fail,<<"Failed pattern match">>}))
          end
      end)
  end).
test51(V) ->
  case (first =:= (erlang:element(1, V))) of
    true ->
      (erlang:element(2, V));
    _ ->
      (erlang:throw({fail,<<"Failed pattern match">>}))
  end.
test51() ->
  (fun
    (V) ->
      case (first =:= (erlang:element(1, V))) of
        true ->
          (erlang:element(2, V));
        _ ->
          (erlang:throw({fail,<<"Failed pattern match">>}))
      end
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
test4() ->
  (fun
    (V) ->
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
      end
  end).
test3(V) ->
  case ((erlang:element(2, V)) < (erlang:element(4, V))) of
    true ->
      (erlang:element(2, V));
    _ ->
      (erlang:element(3, V))
  end.
test3() ->
  (fun
    (V) ->
      case ((erlang:element(2, V)) < (erlang:element(4, V))) of
        true ->
          (erlang:element(2, V));
        _ ->
          (erlang:element(3, V))
      end
  end).
test2(V) ->
  (erlang:element(2, V)).
test2() ->
  (fun
    (V) ->
      (erlang:element(2, V))
  end).
test1(V) ->
  true.
test1() ->
  (fun
    (_) ->
      true
  end).
'don\'tInlineMeMe'(A) ->
  A.
'don\'tInlineMeMe'() ->
  (fun
    (A) ->
      A
  end).
result() ->
  #{test1 => (((snapshot_constructorAccessor@ps:'don\'tInlineMeMe'())((snapshot_constructorAccessor@ps:test1())))({noArgs})),
  test2 => (((snapshot_constructorAccessor@ps:'don\'tInlineMeMe'())((snapshot_constructorAccessor@ps:test2())))({hasArgs,2,1,0})),
  test3 => (((snapshot_constructorAccessor@ps:'don\'tInlineMeMe'())((snapshot_constructorAccessor@ps:test3())))({hasArgs,5,3,1})),
  test4 => (((snapshot_constructorAccessor@ps:'don\'tInlineMeMe'())((snapshot_constructorAccessor@ps:test4())))({last,4})),
  test5 => (((snapshot_constructorAccessor@ps:'don\'tInlineMeMe'())((snapshot_constructorAccessor@ps:test51())))({first,5}))}.

-module(snapshot_constructorAccessor@ps).
-export(['First'/0, 'Last'/0, 'NoArgs'/0, 'HasArgs'/0, 'Foo'/0, z/0, y/0, x/0, test5/0, test51/0, test4/0, test3/0, test2/0, test1/0, 'don\'tInlineMeMe'/0, result/0]).
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
  ((('Foo'())(1))(1)).
x() ->
  (('Foo'())(1)).
test5() ->
  (fun
    (_@dollar__unused@0) ->
      (fun
        (V@1) ->
          case (first =:= (erlang:element(1, V@1))) of
            true ->
              (erlang:element(2, V@1));
            _ ->
              (erlang:throw({fail,<<"Failed pattern match">>}))
          end
      end)
  end).
test51() ->
  (fun
    (V@0) ->
      case (first =:= (erlang:element(1, V@0))) of
        true ->
          (erlang:element(2, V@0));
        _ ->
          (erlang:throw({fail,<<"Failed pattern match">>}))
      end
  end).
test4() ->
  (fun
    (V@0) ->
      case (first =:= (erlang:element(1, V@0))) of
        true ->
          (erlang:element(2, V@0));
        _ ->
          case (last =:= (erlang:element(1, V@0))) of
            true ->
              (erlang:element(2, V@0));
            _ ->
              (erlang:throw({fail,<<"Failed pattern match">>}))
          end
      end
  end).
test3() ->
  (fun
    (V@0) ->
      case ((erlang:element(2, V@0)) < (erlang:element(4, V@0))) of
        true ->
          (erlang:element(2, V@0));
        _ ->
          (erlang:element(3, V@0))
      end
  end).
test2() ->
  (fun
    (V@0) ->
      (erlang:element(2, V@0))
  end).
test1() ->
  (fun
    (V@0) ->
      true
  end).
'don\'tInlineMeMe'() ->
  (fun
    (A@0) ->
      A@0
  end).
result() ->
  #{test1 => ((('don\'tInlineMeMe'())((test1())))(('NoArgs'()))),
  test2 => ((('don\'tInlineMeMe'())((test2())))({hasArgs,2,1,0})),
  test3 => ((('don\'tInlineMeMe'())((test3())))({hasArgs,5,3,1})),
  test4 => ((('don\'tInlineMeMe'())((test4())))({last,4})),
  test5 => ((('don\'tInlineMeMe'())((test51())))({first,5}))}.

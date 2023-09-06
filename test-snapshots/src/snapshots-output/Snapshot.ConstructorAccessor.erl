-module(snapshot_constructorAccessor).
?
?
?
?
?
z() -> 
  ?ctor_saturated.
y() -> 
  (((snapshot_constructorAccessor:'Foo'())(1))(1)).
x() -> 
  ((snapshot_constructorAccessor:'Foo'())(1)).
test5() -> 
  (fun
    ($__unused@0) -> 
      (fun
        (V@1) -> 
          case ?istag of
            true ->
              ?GetField;
            _ ->
              ?fail
          end
      end)
  end).
test4() -> 
  (fun
    (V@0) -> 
      case ?istag of
        true ->
          ?GetField;
        _ ->
          case ?istag of
            true ->
              ?GetField;
            _ ->
              ?fail
          end
      end
  end).
test3() -> 
  (fun
    (V@0) -> 
      case (?GetField < ?GetField) of
        true ->
          ?GetField;
        _ ->
          ?GetField
      end
  end).
test2() -> 
  (fun
    (V@0) -> 
      ?GetField
  end).
test1() -> 
  (fun
    (V@0) -> 
      true
  end).

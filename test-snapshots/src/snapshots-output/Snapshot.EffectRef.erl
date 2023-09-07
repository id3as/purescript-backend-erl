-module(snapshot_effectRef).
positionZero() -> 
  ?prim_effect.
onLet() -> 
  (fun
    (X@0) -> 
      begin
        A@1 = (X@0 + X@0),
        V@2 = (A@1 + (A@1 + X@0)),
        ?prim_effect
      end
  end).
onLetTest() -> 
  begin
    V@0 = ((snapshot_effectRef:onLet())(1)),
    (fun
      () -> 
        begin
          N@1 = (V@0()),
          V@2 = (?prim_effect()),
          (((test_assert:assert())((V@2 =:= 5)))())
        end
    end)
  end.
basicTest() -> 
  (fun
    () -> 
      begin
        N@0 = (?prim_effect()),
        V@1 = (?prim_effect()),
        A'@2 = (?prim_effect()),
        V@3 = (?prim_effect()),
        (((test_assert:assert())((V@3 =:= 1)))())
      end
  end).
main() -> 
  (fun
    () -> 
      begin
        _@dollar__unused@0 = ((snapshot_effectRef:basicTest())()),
        ((snapshot_effectRef:onLetTest())())
      end
  end).

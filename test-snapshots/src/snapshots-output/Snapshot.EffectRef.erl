-module(snapshot_effectRef).
positionZero() -> 
  ?prim_effect.
onLet() -> 
  (fun
    (X@0) -> 
      begin
        A@1 = (X@0 + X@0),
        begin
          V@2 = (A@1 + (A@1 + X@0)),
          ?prim_effect
        end
      end
  end).
onLetTest() -> 
  begin
    V@0 = ((snapshot_effectRef:onLet())(1)),
    ?effect_bind
  end.
basicTest() -> 
  ?effect_bind.
main() -> 
  ?effect_bind.

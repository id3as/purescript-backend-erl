-module(snapshot_effectRef).
-compile(export_all).
positionZero() ->
  ((effect_ref:new())(0)).
onLet() ->
  (fun
    (X@0) ->
      begin
        A@1 = (X@0 + X@0),
        ((effect_ref:new())((A@1 + (A@1 + X@0))))
      end
  end).
onLetTest() ->
  begin
    V@0 = ((snapshot_effectRef:onLet())(1)),
    (fun
      () ->
        begin
          N@1 = (V@0()),
          V@2 = ((fun
            () ->
              {}
          end)()),
          (((test_assert:assert())((V@2 =:= 5)))())
        end
    end)
  end.
basicTest() ->
  begin
    V@0 = ((effect_ref:new())(0)),
    (fun
      () ->
        begin
          N@1 = (V@0()),
          V@2 = ((fun
            () ->
              {}
          end)()),
          A_@prime@3 = ((fun
            () ->
              {}
          end)()),
          V@4 = ((fun
            () ->
              {}
          end)()),
          (((test_assert:assert())((V@4 =:= 1)))())
        end
    end)
  end.
main() ->
  (fun
    () ->
      begin
        _@dollar__unused@0 = ((snapshot_effectRef:basicTest())()),
        ((snapshot_effectRef:onLetTest())())
      end
  end).

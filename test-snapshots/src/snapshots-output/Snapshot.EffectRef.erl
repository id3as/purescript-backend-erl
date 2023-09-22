-module(snapshot_effectRef@ps).
-export([positionZero/0, onLet/0, onLetTest/0, basicTest/0, main/0]).
positionZero() ->
  ((effect_ref@ps:new())(0)).
onLet() ->
  (fun
    (X@0) ->
      begin
        A@1 = (X@0 + X@0),
        ((effect_ref@ps:new())(((A@1 + A@1) + X@0)))
      end
  end).
onLetTest() ->
  begin
    V@0 = ((snapshot_effectRef@ps:onLet())(1)),
    (fun
      () ->
        begin
          N@1 = (V@0()),
          V@2 = (((effect_ref@ps:read())(N@1))()),
          (((test_assert@ps:assert())((V@2 =:= 5)))())
        end
    end)
  end.
basicTest() ->
  begin
    V@0 = ((effect_ref@ps:new())(0)),
    (fun
      () ->
        begin
          N@1 = (V@0()),
          _@dollar__unused@2 = ((((effect_ref@ps:modify_())((fun
            (V@2) ->
              (V@2 + 1)
          end)))(N@1))()),
          V@3 = (((effect_ref@ps:read())(N@1))()),
          (((test_assert@ps:assert())((V@3 =:= 1)))())
        end
    end)
  end.
main() ->
  (fun
    () ->
      begin
        _@dollar__unused@0 = ((snapshot_effectRef@ps:basicTest())()),
        ((snapshot_effectRef@ps:onLetTest())())
      end
  end).

-module(snapshot_effectRef@ps).
-export([positionZero/0, onLet/0, onLetTest/0, basicTest/0, main/0]).
positionZero() ->
  ((effect_ref@ps:new())(0)).
onLet() ->
  (fun
    (X) ->
      begin
        A = (X + X),
        ((effect_ref@ps:new())(((A + A) + X)))
      end
  end).
onLetTest() ->
  begin
    V = ((snapshot_effectRef@ps:onLet())(1)),
    (fun
      () ->
        begin
          N = (V()),
          V@1 = (((effect_ref@ps:read())(N))()),
          (((test_assert@ps:assert())((V@1 =:= 5)))())
        end
    end)
  end.
basicTest() ->
  begin
    V = ((effect_ref@ps:new())(0)),
    (fun
      () ->
        begin
          N = (V()),
          _ = ((((effect_ref@ps:modify_())((fun
            (V@1) ->
              (V@1 + 1)
          end)))(N))()),
          V@1 = (((effect_ref@ps:read())(N))()),
          (((test_assert@ps:assert())((V@1 =:= 1)))())
        end
    end)
  end.
main() ->
  (fun
    () ->
      begin
        _ = ((snapshot_effectRef@ps:basicTest())()),
        ((snapshot_effectRef@ps:onLetTest())())
      end
  end).

-module(snapshot_effectRef@ps).
-export([positionZero/0, onLet/0, onLet/1, onLetTest/0, basicTest/0, main/0]).
-compile(no_auto_import).
positionZero() ->
  effect_ref@foreign:new(0).

onLet() ->
  fun
    (X@Local) ->
      onLet(X@Local)
  end.

onLet(X) ->
  begin
    A = (X + X),
    effect_ref@foreign:new((A + A) + X)
  end.

onLetTest() ->
  begin
    V = (onLet(1)),
    fun
      () ->
        begin
          N = (V()),
          V@1 = ((effect_ref@foreign:read(N))()),
          ((test_assert@ps:assert())(V@1 =:= 5))()
        end
    end
  end.

basicTest() ->
  begin
    V = (effect_ref@foreign:new(0)),
    fun
      () ->
        begin
          N = (V()),
          (effect_ref@ps:modify_(
             fun
               (V@1) ->
                 V@1 + 1
             end,
             N
           ))(),
          V@1 = ((effect_ref@foreign:read(N))()),
          ((test_assert@ps:assert())(V@1 =:= 1))()
        end
    end
  end.

main() ->
  fun
    () ->
      begin
        (basicTest())(),
        (onLetTest())()
      end
  end.


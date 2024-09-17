% Snapshot.EffectRef
-module(snapshot_effectRef@ps).
-export([ positionZero/0
        , onLet/0
        , onLet/1
        , 'onLetTest.0'/0
        , onLetTest/0
        , 'basicTest.0'/0
        , basicTest/0
        , main/0
        ]).
-compile(no_auto_import).
positionZero() ->
  effect_ref@foreign:new(0).

onLet() ->
  fun onLet/1.

onLet(X) ->
  begin
    A = X + X,
    effect_ref@foreign:new((A + A) + X)
  end.

'onLetTest.0'() ->
  onLet(1).

onLetTest() ->
  fun
    () ->
      begin
        N = ('onLetTest.0'())(),
        V = (effect_ref@foreign:read(N))(),
        ((test_assert@ps:assert())(V =:= 5))()
      end
  end.

'basicTest.0'() ->
  effect_ref@foreign:new(0).

basicTest() ->
  fun
    () ->
      begin
        N = ('basicTest.0'())(),
        (effect_ref@ps:modify_(
           fun
             (V) ->
               V + 1
           end,
           N
         ))(),
        V = (effect_ref@foreign:read(N))(),
        ((test_assert@ps:assert())(V =:= 1))()
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


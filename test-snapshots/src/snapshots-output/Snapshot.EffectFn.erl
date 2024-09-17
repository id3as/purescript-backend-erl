% Snapshot.EffectFn
-module(snapshot_effectFn@ps).
-export([ x/0
        , x/1
        , y/0
        , y/2
        , z/0
        , z/2
        , main1/0
        , callY/0
        , callY/1
        , main/0
        , main2/0
        ]).
-compile(no_auto_import).
x() ->
  fun x/1.

x(A) ->
  fun
    (_, _) ->
      (effect_console@foreign:log(A))()
  end.

y() ->
  fun
    (A, B) ->
      y(A, B)
  end.

y(A, B) ->
  (x(<<"Hi">>))(A, B).

z() ->
  fun
    (A, B) ->
      z(A, B)
  end.

z(A, B) ->
  (x(<<"Hi">>))(A, B).

main1() ->
  fun
    () ->
      begin
        y(unit, unit),
        unit
      end
  end.

callY() ->
  fun callY/1.

callY(F) ->
  fun
    () ->
      begin
        F(<<"0">>, <<"1">>),
        F(<<"2">>, <<"3">>)
      end
  end.

main() ->
  fun
    () ->
      begin
        y(unit, unit),
        (callY(fun y/2))()
      end
  end.

main2() ->
  fun
    () ->
      begin
        y(unit, unit),
        (callY(fun z/2))()
      end
  end.


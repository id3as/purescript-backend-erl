-module(snapshot_nestedDo@ps).
-export([ pure_/0
        , 'renamed.0'/0
        , renamed/0
        , 'renamed0.0'/0
        , renamed0/0
        , 'renamed1.0'/0
        , renamed1/0
        , 'renamed2.0'/0
        , renamed2/0
        , 'do1.0'/0
        , do1/0
        , 'do0.0'/0
        , do0/0
        , 'bug26.0'/0
        , bug26/0
        , pure_/1
        ]).
-compile(no_auto_import).
pure_() ->
  effect@ps:pureE().

'renamed.0'() ->
  pure_(unit).

renamed() ->
  fun
    () ->
      begin
        A0 = ('renamed.0'())(),
        A1 = (pure_(A0))(),
        (pure_(A1))(),
        (effect_console@foreign:log(<<"Oh no">>))()
      end
  end.

'renamed0.0'() ->
  pure_(unit).

renamed0() ->
  fun
    () ->
      begin
        A0 = ('renamed0.0'())(),
        A = (pure_(A0))(),
        (pure_(A))(),
        (effect_console@foreign:log(<<"Oh no">>))()
      end
  end.

'renamed1.0'() ->
  pure_(unit).

renamed1() ->
  fun
    () ->
      begin
        A = ('renamed1.0'())(),
        A@1 = (pure_(A))(),
        (pure_(A@1))(),
        (effect_console@foreign:log(<<"Oh no">>))()
      end
  end.

'renamed2.0'() ->
  pure_(unit).

renamed2() ->
  fun
    () ->
      begin
        A = ('renamed2.0'())(),
        A1 = (pure_(A))(),
        (pure_(A1))(),
        (effect_console@foreign:log(<<"Oh no">>))()
      end
  end.

'do1.0'() ->
  pure_(unit).

do1() ->
  fun
    () ->
      begin
        A = ('do1.0'())(),
        (pure_(A))(),
        (effect_console@foreign:log(<<"Oh no">>))()
      end
  end.

'do0.0'() ->
  pure_(unit).

do0() ->
  fun
    () ->
      begin
        ('do0.0'())(),
        (effect_console@foreign:log(<<"Oh no">>))()
      end
  end.

'bug26.0'() ->
  pure_(unit).

bug26() ->
  fun
    () ->
      begin
        A = ('bug26.0'())(),
        A@1 = (pure_(A))(),
        (pure_(A@1))(),
        (effect_console@foreign:log(<<"Oh no">>))()
      end
  end.

pure_(V) ->
  fun
    () ->
      V
  end.


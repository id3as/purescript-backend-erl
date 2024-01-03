-module(snapshot_nestedDo@ps).
-export([ pure_/0
        , renamed/0
        , renamed0/0
        , renamed1/0
        , renamed2/0
        , do1/0
        , do0/0
        , bug26/0
        , pure_/1
        ]).
-compile(no_auto_import).
pure_() ->
  effect@ps:pureE().

renamed() ->
  begin
    V = pure_(unit),
    fun
      () ->
        begin
          A0 = V(),
          A1 = (pure_(A0))(),
          (pure_(A1))(),
          (effect_console@foreign:log(<<"Oh no">>))()
        end
    end
  end.

renamed0() ->
  begin
    V = pure_(unit),
    fun
      () ->
        begin
          A0 = V(),
          A = (pure_(A0))(),
          (pure_(A))(),
          (effect_console@foreign:log(<<"Oh no">>))()
        end
    end
  end.

renamed1() ->
  begin
    V = pure_(unit),
    fun
      () ->
        begin
          A = V(),
          A@1 = (pure_(A))(),
          (pure_(A@1))(),
          (effect_console@foreign:log(<<"Oh no">>))()
        end
    end
  end.

renamed2() ->
  begin
    V = pure_(unit),
    fun
      () ->
        begin
          A = V(),
          A1 = (pure_(A))(),
          (pure_(A1))(),
          (effect_console@foreign:log(<<"Oh no">>))()
        end
    end
  end.

do1() ->
  begin
    V = pure_(unit),
    fun
      () ->
        begin
          A = V(),
          (pure_(A))(),
          (effect_console@foreign:log(<<"Oh no">>))()
        end
    end
  end.

do0() ->
  begin
    V = pure_(unit),
    fun
      () ->
        begin
          V(),
          (effect_console@foreign:log(<<"Oh no">>))()
        end
    end
  end.

bug26() ->
  begin
    V = pure_(unit),
    fun
      () ->
        begin
          A = V(),
          A@1 = (pure_(A))(),
          (pure_(A@1))(),
          (effect_console@foreign:log(<<"Oh no">>))()
        end
    end
  end.

pure_(V) ->
  fun
    () ->
      V
  end.


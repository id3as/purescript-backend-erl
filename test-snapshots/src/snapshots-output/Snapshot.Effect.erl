-module(snapshot_effect@ps).
-export([ 'don\'tInlineMeMe'/0
        , 'don\'tInlineMeMe'/1
        , 'lastComponentIsRun.0'/0
        , lastComponentIsRun/0
        , 'lastPureIsUnwrapped.0'/0
        , lastPureIsUnwrapped/0
        , main/0
        ]).
-compile(no_auto_import).
'don\'tInlineMeMe'() ->
  fun
    (V) ->
      'don\'tInlineMeMe'(V)
  end.

'don\'tInlineMeMe'(_) ->
  fun
    () ->
      unit
  end.

'lastComponentIsRun.0'() ->
  'don\'tInlineMeMe'(<<"a">>).

lastComponentIsRun() ->
  fun
    () ->
      begin
        ('lastComponentIsRun.0'())(),
        ('don\'tInlineMeMe'(<<"b">>))(),
        ('don\'tInlineMeMe'(<<"c">>))()
      end
  end.

'lastPureIsUnwrapped.0'() ->
  'don\'tInlineMeMe'(<<"a">>).

lastPureIsUnwrapped() ->
  fun
    () ->
      begin
        Value = ('lastPureIsUnwrapped.0'())(),
        ('don\'tInlineMeMe'(<<"b">>))(),
        Value
      end
  end.

main() ->
  fun
    () ->
      begin
        (lastComponentIsRun())(),
        (lastPureIsUnwrapped())()
      end
  end.


% Snapshot.Effect
-module(snapshot_effect@ps).
-export([ 'don\'tInlineMeMe'/0
        , 'don\'tInlineMeMe'/1
        , 'lastComponentIsRun.0'/0
        , lastComponentIsRun/0
        , 'lastPureIsUnwrapped.0'/0
        , lastPureIsUnwrapped/0
        , doWhen/0
        , doWhen/1
        , caseInChain/0
        , caseInChain/1
        , 'main.0'/0
        , main/0
        ]).
-compile(no_auto_import).
'don\'tInlineMeMe'() ->
  fun 'don\'tInlineMeMe'/1.

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

doWhen() ->
  fun doWhen/1.

doWhen(B) ->
  begin
    V =
      if
        B ->
          'don\'tInlineMeMe'(<<"b">>);
        true ->
          fun
            () ->
              unit
          end
      end,
    fun
      () ->
        begin
          V(),
          if
            not B ->
              ('don\'tInlineMeMe'(<<"not b">>))();
            true ->
              unit
          end
        end
    end
  end.

caseInChain() ->
  fun caseInChain/1.

caseInChain(I) ->
  fun
    () ->
      begin
        V = I + 4,
        V@1 =
          if
            V =:= 0 ->
              0;
            true ->
              begin
                ('don\'tInlineMeMe'(<<"x">>))(),
                V - 4
              end
          end,
        ('don\'tInlineMeMe'(<<"y">>))(),
        V1 = ((erlang:map_get(compare, data_ord@ps:ordInt()))(I))(8),
        case V1 of
          {eQ} ->
            unit;
          {lT} ->
            ('don\'tInlineMeMe'(<<"LT">>))();
          {gT} ->
            ('don\'tInlineMeMe'(<<"GT">>))();
          _ ->
            (erlang:error({fail, <<"Failed pattern match">>}))()
        end,
        V@1
      end
  end.

'main.0'() ->
  caseInChain(3).

main() ->
  fun
    () ->
      begin
        ('main.0'())(),
        (doWhen(true))(),
        (doWhen(false))(),
        (lastComponentIsRun())(),
        (lastPureIsUnwrapped())()
      end
  end.


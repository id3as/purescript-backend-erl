-module(snapshot_state_log@ps).
-export([freshE/0, freshE/1, 'exE\''/0, 'exE\''/1, exE/0, exE/1]).
-compile(no_auto_import).
freshE() ->
  fun
    (DictMonadState) ->
      freshE(DictMonadState)
  end.

freshE(#{ state := DictMonadState@1 }) ->
  DictMonadState@1(fun
    (S) ->
      {tuple, S, S + 1}
  end).

'exE\''() ->
  fun
    (S) ->
      'exE\''(S)
  end.

'exE\''(S) ->
  begin
    V = S + 1,
    fun
      () ->
        begin
          (effect_console@foreign:log(data_show@foreign:showIntImpl(S)))(),
          (effect_console@foreign:log(data_show@foreign:showIntImpl(V)))(),
          {tuple, #{ a => S, b => V }, V + 1}
        end
    end
  end.

exE() ->
  fun
    (S) ->
      exE(S)
  end.

exE(S) ->
  begin
    V = S + 1,
    fun
      () ->
        begin
          (effect_console@foreign:log(data_show@foreign:showIntImpl(S)))(),
          (effect_console@foreign:log(data_show@foreign:showIntImpl(V)))(),
          {tuple, #{ a => S, b => V }, V + 1}
        end
    end
  end.


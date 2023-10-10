-module(snapshot_state_log@ps).
-export([freshE/0, freshE/1, 'exE\''/0, 'exE\''/1, exE/0, exE/1]).
-compile(no_auto_import).
freshE() ->
  fun
    (DictMonadState@Local) ->
      freshE(DictMonadState@Local)
  end.

freshE(DictMonadState) ->
  (erlang:map_get(state, DictMonadState))
  (fun
    (S) ->
      {tuple, S, S + 1}
  end).

'exE\''() ->
  fun
    (S@Local) ->
      'exE\''(S@Local)
  end.

'exE\''(S) ->
  begin
    V = (S + 1),
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
    (S@Local) ->
      exE(S@Local)
  end.

exE(S) ->
  begin
    V = (S + 1),
    fun
      () ->
        begin
          (effect_console@foreign:log(data_show@foreign:showIntImpl(S)))(),
          (effect_console@foreign:log(data_show@foreign:showIntImpl(V)))(),
          {tuple, #{ a => S, b => V }, V + 1}
        end
    end
  end.


-module(snapshot_state_log@ps).
-export([freshE/1, freshE/0, 'exE\''/1, 'exE\''/0, exE/1, exE/0]).
freshE(DictMonadState) ->
  ((maps:get(state, DictMonadState))((fun
    (S) ->
      {tuple,S,(S + 1)}
  end))).
freshE() ->
  (fun
    (DictMonadState) ->
      ((maps:get(state, DictMonadState))((fun
        (S) ->
          {tuple,S,(S + 1)}
      end)))
  end).
'exE\''(S) ->
  begin
    V = (S + 1),
    (fun
      () ->
        begin
          _ = (((effect_console@ps:log())(((data_show@ps:showIntImpl())(S))))()),
          _ = (((effect_console@ps:log())(((data_show@ps:showIntImpl())(V))))()),
          {tuple,#{a => S,
          b => V},(V + 1)}
        end
    end)
  end.
'exE\''() ->
  (fun
    (S) ->
      begin
        V = (S + 1),
        (fun
          () ->
            begin
              _ = (((effect_console@ps:log())(((data_show@ps:showIntImpl())(S))))()),
              _ = (((effect_console@ps:log())(((data_show@ps:showIntImpl())(V))))()),
              {tuple,#{a => S,
              b => V},(V + 1)}
            end
        end)
      end
  end).
exE(S) ->
  begin
    V = (S + 1),
    (fun
      () ->
        begin
          _ = (((effect_console@ps:log())(((data_show@ps:showIntImpl())(S))))()),
          _ = (((effect_console@ps:log())(((data_show@ps:showIntImpl())(V))))()),
          {tuple,#{a => S,
          b => V},(V + 1)}
        end
    end)
  end.
exE() ->
  (fun
    (S) ->
      begin
        V = (S + 1),
        (fun
          () ->
            begin
              _ = (((effect_console@ps:log())(((data_show@ps:showIntImpl())(S))))()),
              _ = (((effect_console@ps:log())(((data_show@ps:showIntImpl())(V))))()),
              {tuple,#{a => S,
              b => V},(V + 1)}
            end
        end)
      end
  end).

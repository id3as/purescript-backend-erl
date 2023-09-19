-module(snapshot_state_log@ps).
-export([freshE/0, 'exE\''/0, exE/0]).
freshE() ->
  (fun
    (DictMonadState@0) ->
      ((maps:get(state, DictMonadState@0))((fun
        (S@1) ->
          {tuple,S@1,(S@1 + 1)}
      end)))
  end).
'exE\''() ->
  (fun
    (S@0) ->
      begin
        V@1 = (S@0 + 1),
        (fun
          () ->
            begin
              X@2 = (((effect_console@ps:log())(((data_show@ps:showIntImpl())(S@0))))()),
              X@3 = (((effect_console@ps:log())(((data_show@ps:showIntImpl())(V@1))))()),
              {tuple,#{a => S@0,
              b => V@1},(V@1 + 1)}
            end
        end)
      end
  end).
exE() ->
  (fun
    (S@0) ->
      begin
        V@1 = (S@0 + 1),
        (fun
          () ->
            begin
              A_@prime@2 = (((effect_console@ps:log())(((data_show@ps:showIntImpl())(S@0))))()),
              X@3 = (((effect_console@ps:log())(((data_show@ps:showIntImpl())(V@1))))()),
              {tuple,#{a => S@0,
              b => V@1},(V@1 + 1)}
            end
        end)
      end
  end).

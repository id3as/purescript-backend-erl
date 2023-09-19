-module(snapshot_state_log@ps).
-export([freshMTL/0, freshE/0, exMTL/0, 'exE\''/0, exE/0]).
freshMTL() ->
  (fun
    (X@0) ->
      {tuple,X@0,(X@0 + 1)}
  end).
freshE() ->
  (fun
    (DictMonadState@0) ->
      ((maps:get(state, DictMonadState@0))((fun
        (S@1) ->
          {tuple,S@1,(S@1 + 1)}
      end)))
  end).
exMTL() ->
  (fun
    (S@0) ->
      begin
        V@1 = (S@0 + 1),
        {tuple,#{a => S@0,
        b => V@1},(V@1 + 1)}
      end
  end).
'exE\''() ->
  (fun
    (S@0) ->
      begin
        V@1 = (S@0 + 1),
        (fun
          () ->
            begin
              V1@2 = (((((maps:get(lift, (control_monad_state_trans@ps:monadTransStateT())))((effect@ps:monadEffect())))(((effect_console@ps:log())(((data_show@ps:showIntImpl())(S@0))))))(V@1))()),
              {tuple,#{a => S@0,
              b => (erlang:element(3, V1@2))},((erlang:element(3, V1@2)) + 1)}
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
              V1@3 = (((((maps:get(lift, (control_monad_state_trans@ps:monadTransStateT())))((effect@ps:monadEffect())))(((effect_console@ps:log())(((data_show@ps:showIntImpl())(V@1))))))((V@1 + 1)))()),
              {tuple,#{a => S@0,
              b => V@1},(erlang:element(3, V1@3))}
            end
        end)
      end
  end).

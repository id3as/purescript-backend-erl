-module(snapshot_state@ps).
-export(['State'/0, functorState/0, freshMTL/0, freshE/0, fresh/0, exMTL/0, 'exE\''/0, exE/0, monadState/0, bindState/0, applyState/0, applicativeState/0, ex/0]).
'State'() ->
  (fun
    (X) ->
      X
  end).
functorState() ->
  #{map => (fun
    (F) ->
      (fun
        (V) ->
          (fun
            (S) ->
              begin
                V1 = (V(S)),
                {tuple,(F((erlang:element(2, V1)))),(erlang:element(3, V1))}
              end
          end)
      end)
  end)}.
freshMTL() ->
  (fun
    (X) ->
      {tuple,X,(X + 1)}
  end).
freshE() ->
  (fun
    (DictMonadState) ->
      ((maps:get(state, DictMonadState))((fun
        (S) ->
          {tuple,S,(S + 1)}
      end)))
  end).
fresh() ->
  (fun
    (S) ->
      {tuple,S,(S + 1)}
  end).
exMTL() ->
  (fun
    (S) ->
      begin
        V = (S + 1),
        {tuple,#{a => S,
        b => V},(V + 1)}
      end
  end).
'exE\''() ->
  (fun
    (S) ->
      begin
        V = (S + 1),
        (fun
          () ->
            {tuple,#{a => S,
            b => V},(V + 1)}
        end)
      end
  end).
exE() ->
  (fun
    (S) ->
      begin
        V = (S + 1),
        (fun
          () ->
            {tuple,#{a => S,
            b => V},(V + 1)}
        end)
      end
  end).
monadState() ->
  #{'Applicative0' => (fun
    (_) ->
      (snapshot_state@ps:applicativeState())
  end),
  'Bind1' => (fun
    (_) ->
      (snapshot_state@ps:bindState())
  end)}.
bindState() ->
  #{bind => (fun
    (V) ->
      (fun
        (F) ->
          (fun
            (S) ->
              begin
                V1 = (V(S)),
                ((F((erlang:element(2, V1))))((erlang:element(3, V1))))
              end
          end)
      end)
  end),
  'Apply0' => (fun
    (_) ->
      (snapshot_state@ps:applyState())
  end)}.
applyState() ->
  #{apply => (fun
    (F) ->
      (fun
        (A) ->
          (fun
            (S) ->
              begin
                V1 = (F(S)),
                V1@1 = (A((erlang:element(3, V1)))),
                (((maps:get(pure, (snapshot_state@ps:applicativeState())))(((erlang:element(2, V1))((erlang:element(2, V1@1))))))((erlang:element(3, V1@1))))
              end
          end)
      end)
  end),
  'Functor0' => (fun
    (_) ->
      (snapshot_state@ps:functorState())
  end)}.
applicativeState() ->
  #{pure => (fun
    (A) ->
      (fun
        (S) ->
          {tuple,A,S}
      end)
  end),
  'Apply0' => (fun
    (_) ->
      (snapshot_state@ps:applyState())
  end)}.
ex() ->
  (fun
    (S) ->
      begin
        V = (S + 1),
        {tuple,#{a => S,
        b => V},(V + 1)}
      end
  end).

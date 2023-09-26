-module(snapshot_state@ps).
-export(['State'/0, 'State'/1, functorState/0, freshMTL/0, freshMTL/1, freshE/0, freshE/1, fresh/0, fresh/1, exMTL/0, exMTL/1, 'exE\''/0, 'exE\''/1, exE/0, exE/1, monadState/0, bindState/0, applyState/0, applicativeState/0, ex/0, ex/1]).
'State'() ->
  (fun
    (V@0) ->
      ('State'(V@0))
  end).
'State'(X) ->
  X.
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
    (V@0) ->
      (freshMTL(V@0))
  end).
freshMTL(X) ->
  {tuple,X,(X + 1)}.
freshE() ->
  (fun
    (V@0) ->
      (freshE(V@0))
  end).
freshE(DictMonadState) ->
  ((maps:get(state, DictMonadState))((fun
    (S) ->
      {tuple,S,(S + 1)}
  end))).
fresh() ->
  (fun
    (V@0) ->
      (fresh(V@0))
  end).
fresh(S) ->
  {tuple,S,(S + 1)}.
exMTL() ->
  (fun
    (V@0) ->
      (exMTL(V@0))
  end).
exMTL(S) ->
  begin
    V = (S + 1),
    {tuple,#{a => S,
    b => V},(V + 1)}
  end.
'exE\''() ->
  (fun
    (V@0) ->
      ('exE\''(V@0))
  end).
'exE\''(S) ->
  begin
    V = (S + 1),
    (fun
      () ->
        {tuple,#{a => S,
        b => V},(V + 1)}
    end)
  end.
exE() ->
  (fun
    (V@0) ->
      (exE(V@0))
  end).
exE(S) ->
  begin
    V = (S + 1),
    (fun
      () ->
        {tuple,#{a => S,
        b => V},(V + 1)}
    end)
  end.
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
    (V@0) ->
      (ex(V@0))
  end).
ex(S) ->
  begin
    V = (S + 1),
    {tuple,#{a => S,
    b => V},(V + 1)}
  end.

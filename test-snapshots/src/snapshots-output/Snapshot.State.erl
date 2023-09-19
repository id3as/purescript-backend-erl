-module(snapshot_state@ps).
-export(['State'/0, functorState/0, freshMTL/0, freshE/0, fresh/0, exMTL/0, 'exE\''/0, exE/0, monadState/0, bindState/0, applyState/0, applicativeState/0, ex/0]).
'State'() ->
  (fun
    (X@0) ->
      X@0
  end).
functorState() ->
  #{map => (fun
    (F@0) ->
      (fun
        (V@1) ->
          (fun
            (S@2) ->
              begin
                V1@3 = (V@1(S@2)),
                {tuple,(F@0((erlang:element(2, V1@3)))),(erlang:element(3, V1@3))}
              end
          end)
      end)
  end)}.
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
fresh() ->
  (fun
    (S@0) ->
      {tuple,S@0,(S@0 + 1)}
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
            {tuple,#{a => S@0,
            b => V@1},(V@1 + 1)}
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
            {tuple,#{a => S@0,
            b => V@1},(V@1 + 1)}
        end)
      end
  end).
monadState() ->
  #{'Applicative0' => (fun
    (_@dollar__unused@0) ->
      (applicativeState())
  end),
  'Bind1' => (fun
    (_@dollar__unused@0) ->
      (bindState())
  end)}.
bindState() ->
  #{bind => (fun
    (V@0) ->
      (fun
        (F@1) ->
          (fun
            (S@2) ->
              begin
                V1@3 = (V@0(S@2)),
                ((F@1((erlang:element(2, V1@3))))((erlang:element(3, V1@3))))
              end
          end)
      end)
  end),
  'Apply0' => (fun
    (_@dollar__unused@0) ->
      (applyState())
  end)}.
applyState() ->
  #{apply => (fun
    (F@0) ->
      (fun
        (A@1) ->
          (fun
            (S@2) ->
              begin
                V1@3 = (F@0(S@2)),
                V1@4 = (A@1((erlang:element(3, V1@3)))),
                (((maps:get(pure, (applicativeState())))(((erlang:element(2, V1@3))((erlang:element(2, V1@4))))))((erlang:element(3, V1@4))))
              end
          end)
      end)
  end),
  'Functor0' => (fun
    (_@dollar__unused@0) ->
      (functorState())
  end)}.
applicativeState() ->
  #{pure => (fun
    (A@0) ->
      (fun
        (S@1) ->
          {tuple,A@0,S@1}
      end)
  end),
  'Apply0' => (fun
    (_@dollar__unused@0) ->
      (applyState())
  end)}.
ex() ->
  (fun
    (S@0) ->
      begin
        V@1 = (S@0 + 1),
        {tuple,#{a => S@0,
        b => V@1},(V@1 + 1)}
      end
  end).

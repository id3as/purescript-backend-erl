-module(snapshot_state@ps).
-export([ 'State'/0
        , 'State'/1
        , functorState/0
        , freshMTL/0
        , freshMTL/1
        , freshE/0
        , freshE/1
        , fresh/0
        , fresh/1
        , exMTL/0
        , exMTL/1
        , 'exE\''/0
        , 'exE\''/1
        , exE/0
        , exE/1
        , monadState/0
        , bindState/0
        , applyState/0
        , applicativeState/0
        , ex/0
        , ex/1
        ]).
-compile(no_auto_import).
'State'() ->
  fun
    (X) ->
      'State'(X)
  end.

'State'(X) ->
  X.

functorState() ->
  #{ map =>
     fun
       (F) ->
         fun
           (V) ->
             fun
               (S) ->
                 begin
                   V1 = V(S),
                   {tuple, F(erlang:element(2, V1)), erlang:element(3, V1)}
                 end
             end
         end
     end
   }.

freshMTL() ->
  fun
    (X) ->
      freshMTL(X)
  end.

freshMTL(X) ->
  {tuple, X, X + 1}.

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

fresh() ->
  fun
    (S) ->
      fresh(S)
  end.

fresh(S) ->
  {tuple, S, S + 1}.

exMTL() ->
  fun
    (S) ->
      exMTL(S)
  end.

exMTL(S) ->
  begin
    V = S + 1,
    {tuple, #{ a => S, b => V }, V + 1}
  end.

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
        {tuple, #{ a => S, b => V }, V + 1}
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
        {tuple, #{ a => S, b => V }, V + 1}
    end
  end.

monadState() ->
  #{ 'Applicative0' =>
     fun
       (_) ->
         applicativeState()
     end
   , 'Bind1' =>
     fun
       (_) ->
         bindState()
     end
   }.

bindState() ->
  #{ bind =>
     fun
       (V) ->
         fun
           (F) ->
             fun
               (S) ->
                 begin
                   V1 = V(S),
                   (F(erlang:element(2, V1)))(erlang:element(3, V1))
                 end
             end
         end
     end
   , 'Apply0' =>
     fun
       (V) ->
         applyState()
     end
   }.

applyState() ->
  #{ apply =>
     fun
       (F) ->
         fun
           (A) ->
             fun
               (S) ->
                 begin
                   V1 = F(S),
                   V1@1 = A(erlang:element(3, V1)),
                   ((erlang:map_get(pure, applicativeState()))
                    ((erlang:element(2, V1))(erlang:element(2, V1@1))))
                   (erlang:element(3, V1@1))
                 end
             end
         end
     end
   , 'Functor0' =>
     fun
       (_) ->
         functorState()
     end
   }.

applicativeState() ->
  #{ pure =>
     fun
       (A) ->
         fun
           (S) ->
             {tuple, A, S}
         end
     end
   , 'Apply0' =>
     fun
       (_) ->
         applyState()
     end
   }.

ex() ->
  fun
    (S) ->
      ex(S)
  end.

ex(S) ->
  begin
    V = S + 1,
    {tuple, #{ a => S, b => V }, V + 1}
  end.


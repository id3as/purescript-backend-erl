-module(snapshot_constructor@ps).
-export([ 'Nil'/0
        , 'Node'/0
        , 'Stop1'/0
        , 'Continue1'/0
        , 'Stop2'/0
        , 'Continue2'/0
        , 'Just'/0
        , 'Nothing'/0
        ]).
-compile(no_auto_import).
'Nil'() ->
  {nil}.

'Node'() ->
  fun
    (Value0) ->
      fun
        (Value1) ->
          fun
            (Value2) ->
              {node, Value0, Value1, Value2}
          end
      end
  end.

'Stop1'() ->
  {stop1}.

'Continue1'() ->
  fun
    (Value0) ->
      {continue1, Value0}
  end.

'Stop2'() ->
  {stop2}.

'Continue2'() ->
  fun
    (Value0) ->
      {continue2, Value0}
  end.

'Just'() ->
  fun
    (Value0) ->
      {just, Value0}
  end.

'Nothing'() ->
  {nothing}.


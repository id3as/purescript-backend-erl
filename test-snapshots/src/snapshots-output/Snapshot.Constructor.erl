-module(snapshot_constructor).
-compile(export_all).
'Nil'() -> 
  {nil}.
'Node'() -> 
  (fun
    (Value0) ->
      (fun
        (Value1) ->
          (fun
            (Value2) ->
              {node,Value0,Value1,Value2}
          end)
      end)
  end).
'Stop1'() -> 
  {stop1}.
'Continue1'() -> 
  (fun
    (Value0) ->
      {continue1,Value0}
  end).
'Stop2'() -> 
  {stop2}.
'Continue2'() -> 
  (fun
    (Value0) ->
      {continue2,Value0}
  end).
'Just'() -> 
  (fun
    (Value0) ->
      {just,Value0}
  end).
'Nothing'() -> 
  {nothing}.

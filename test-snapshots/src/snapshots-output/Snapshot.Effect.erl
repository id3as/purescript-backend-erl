-module(snapshot_effect).
dontInlineMe() -> 
  (fun
    (V@0) -> 
      ?effect_pure
  end).
lastComponentIsRun() -> 
  begin
    V@0 = ((snapshot_effect:dontInlineMe())(<<"a">>)),
    ?effect_bind
  end.
lastPureIsUnwrapped() -> 
  begin
    V@0 = ((snapshot_effect:dontInlineMe())(<<"a">>)),
    ?effect_bind
  end.
main() -> 
  ?effect_bind.

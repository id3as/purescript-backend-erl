-module(snapshot_effect).
dontInlineMe() -> 
  (fun
    (V@0) -> 
      (fun
        () -> 
          (data_unit:unit())
      end)
  end).
lastComponentIsRun() -> 
  begin
    V@0 = ((snapshot_effect:dontInlineMe())(<<"a">>)),
    (fun
      () -> 
        begin
          _@dollar__unused@1 = (V@0()),
          _@dollar__unused@2 = (((snapshot_effect:dontInlineMe())(<<"b">>))()),
          (((snapshot_effect:dontInlineMe())(<<"c">>))())
        end
    end)
  end.
lastPureIsUnwrapped() -> 
  begin
    V@0 = ((snapshot_effect:dontInlineMe())(<<"a">>)),
    (fun
      () -> 
        begin
          Value@1 = (V@0()),
          _@dollar__unused@2 = (((snapshot_effect:dontInlineMe())(<<"b">>))()),
          Value@1
        end
    end)
  end.
main() -> 
  (fun
    () -> 
      begin
        _@dollar__unused@0 = ((snapshot_effect:lastComponentIsRun())()),
        ((snapshot_effect:lastPureIsUnwrapped())())
      end
  end).

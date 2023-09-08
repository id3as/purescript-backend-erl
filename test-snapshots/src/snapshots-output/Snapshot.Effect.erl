-module(snapshot_effect@ps).
-compile(export_all).
dontInlineMe() ->
  (fun
    (V@0) ->
      (fun
        () ->
          (data_unit@ps:unit())
      end)
  end).
lastComponentIsRun() ->
  begin
    V@0 = ((snapshot_effect@ps:dontInlineMe())(<<"a">>)),
    (fun
      () ->
        begin
          _@dollar__unused@1 = (V@0()),
          _@dollar__unused@2 = (((snapshot_effect@ps:dontInlineMe())(<<"b">>))()),
          (((snapshot_effect@ps:dontInlineMe())(<<"c">>))())
        end
    end)
  end.
lastPureIsUnwrapped() ->
  begin
    V@0 = ((snapshot_effect@ps:dontInlineMe())(<<"a">>)),
    (fun
      () ->
        begin
          Value@1 = (V@0()),
          _@dollar__unused@2 = (((snapshot_effect@ps:dontInlineMe())(<<"b">>))()),
          Value@1
        end
    end)
  end.
main() ->
  (fun
    () ->
      begin
        _@dollar__unused@0 = ((snapshot_effect@ps:lastComponentIsRun())()),
        ((snapshot_effect@ps:lastPureIsUnwrapped())())
      end
  end).

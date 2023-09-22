-module(snapshot_effect@ps).
-export(['don\'tInlineMeMe'/0, lastComponentIsRun/0, lastPureIsUnwrapped/0, main/0]).
'don\'tInlineMeMe'() ->
  (fun
    (_) ->
      (fun
        () ->
          unit
      end)
  end).
lastComponentIsRun() ->
  begin
    V@0 = ((snapshot_effect@ps:'don\'tInlineMeMe'())(<<"a">>)),
    (fun
      () ->
        begin
          _ = (V@0()),
          _ = (((snapshot_effect@ps:'don\'tInlineMeMe'())(<<"b">>))()),
          (((snapshot_effect@ps:'don\'tInlineMeMe'())(<<"c">>))())
        end
    end)
  end.
lastPureIsUnwrapped() ->
  begin
    V@0 = ((snapshot_effect@ps:'don\'tInlineMeMe'())(<<"a">>)),
    (fun
      () ->
        begin
          Value@1 = (V@0()),
          _ = (((snapshot_effect@ps:'don\'tInlineMeMe'())(<<"b">>))()),
          Value@1
        end
    end)
  end.
main() ->
  (fun
    () ->
      begin
        _ = ((snapshot_effect@ps:lastComponentIsRun())()),
        ((snapshot_effect@ps:lastPureIsUnwrapped())())
      end
  end).

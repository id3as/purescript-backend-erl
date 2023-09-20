-module(snapshot_effect@ps).
-export(['don\'tInlineMeMe'/0, lastComponentIsRun/0, lastPureIsUnwrapped/0, main/0]).
'don\'tInlineMeMe'() ->
  (fun
    (V@0) ->
      (fun
        () ->
          unit
      end)
  end).
lastComponentIsRun() ->
  begin
    V@0 = (('don\'tInlineMeMe'())(<<"a">>)),
    (fun
      () ->
        begin
          _@dollar__unused@1 = (V@0()),
          _@dollar__unused@2 = ((('don\'tInlineMeMe'())(<<"b">>))()),
          ((('don\'tInlineMeMe'())(<<"c">>))())
        end
    end)
  end.
lastPureIsUnwrapped() ->
  begin
    V@0 = (('don\'tInlineMeMe'())(<<"a">>)),
    (fun
      () ->
        begin
          Value@1 = (V@0()),
          _@dollar__unused@2 = ((('don\'tInlineMeMe'())(<<"b">>))()),
          Value@1
        end
    end)
  end.
main() ->
  (fun
    () ->
      begin
        _@dollar__unused@0 = ((lastComponentIsRun())()),
        ((lastPureIsUnwrapped())())
      end
  end).

-module(snapshot_effect@ps).
-export(['don\'tInlineMeMe'/0, 'don\'tInlineMeMe'/1, lastComponentIsRun/0, lastPureIsUnwrapped/0, main/0]).
-compile(no_auto_import).
'don\'tInlineMeMe'() ->
  (fun
    (V@Local) ->
      ('don\'tInlineMeMe'(V@Local))
  end).
'don\'tInlineMeMe'(_) ->
  (fun
    () ->
      unit
  end).
lastComponentIsRun() ->
  begin
    V = ((snapshot_effect@ps:'don\'tInlineMeMe'())(<<"a">>)),
    (fun
      () ->
        begin
          _ = (V()),
          _ = (((snapshot_effect@ps:'don\'tInlineMeMe'())(<<"b">>))()),
          (((snapshot_effect@ps:'don\'tInlineMeMe'())(<<"c">>))())
        end
    end)
  end.
lastPureIsUnwrapped() ->
  begin
    V = ((snapshot_effect@ps:'don\'tInlineMeMe'())(<<"a">>)),
    (fun
      () ->
        begin
          Value = (V()),
          _ = (((snapshot_effect@ps:'don\'tInlineMeMe'())(<<"b">>))()),
          Value
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

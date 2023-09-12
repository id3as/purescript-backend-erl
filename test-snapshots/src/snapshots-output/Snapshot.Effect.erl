-module(snapshot_effect@ps).
-export([dontInlineMe/0, lastComponentIsRun/0, lastPureIsUnwrapped/0, main/0]).
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
    V@0 = ((dontInlineMe())(<<"a">>)),
    (fun
      () ->
        begin
          _@dollar__unused@1 = (V@0()),
          _@dollar__unused@2 = (((dontInlineMe())(<<"b">>))()),
          (((dontInlineMe())(<<"c">>))())
        end
    end)
  end.
lastPureIsUnwrapped() ->
  begin
    V@0 = ((dontInlineMe())(<<"a">>)),
    (fun
      () ->
        begin
          Value@1 = (V@0()),
          _@dollar__unused@2 = (((dontInlineMe())(<<"b">>))()),
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

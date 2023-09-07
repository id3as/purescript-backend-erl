-module('Snapshot.Effect').
-compile(export_all).
dontInlineMe() -> 
  (fun
    (V@0) ->
      (fun
        () ->
          ('Data.Unit':unit())
      end)
  end).
lastComponentIsRun() -> 
  begin
    V@0 = (('Snapshot.Effect':dontInlineMe())(<<"a"/utf8>>)),
    (fun
      () ->
        begin
          _@dollar__unused@1 = (V@0()),
          _@dollar__unused@2 = ((('Snapshot.Effect':dontInlineMe())(<<"b"/utf8>>))()),
          ((('Snapshot.Effect':dontInlineMe())(<<"c"/utf8>>))())
        end
    end)
  end.
lastPureIsUnwrapped() -> 
  begin
    V@0 = (('Snapshot.Effect':dontInlineMe())(<<"a"/utf8>>)),
    (fun
      () ->
        begin
          Value@1 = (V@0()),
          _@dollar__unused@2 = ((('Snapshot.Effect':dontInlineMe())(<<"b"/utf8>>))()),
          Value@1
        end
    end)
  end.
main() -> 
  (fun
    () ->
      begin
        _@dollar__unused@0 = (('Snapshot.Effect':lastComponentIsRun())()),
        (('Snapshot.Effect':lastPureIsUnwrapped())())
      end
  end).

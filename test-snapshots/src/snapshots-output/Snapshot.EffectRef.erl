-module('Snapshot.EffectRef').
-compile(export_all).
positionZero() -> 
  (fun
    () ->
      {}
  end).
onLet() -> 
  (fun
    (X@0) ->
      begin
        A@1 = (X@0 + X@0),
        V@2 = (A@1 + (A@1 + X@0)),
        (fun
          () ->
            {}
        end)
      end
  end).
onLetTest() -> 
  begin
    V@0 = (('Snapshot.EffectRef':onLet())(1)),
    (fun
      () ->
        begin
          N@1 = (V@0()),
          V@2 = ((fun
            () ->
              {}
          end)()),
          ((('Test.Assert':assert())((V@2 =:= 5)))())
        end
    end)
  end.
basicTest() -> 
  (fun
    () ->
      begin
        N@0 = ((fun
          () ->
            {}
        end)()),
        V@1 = ((fun
          () ->
            {}
        end)()),
        A_@prime@2 = ((fun
          () ->
            {}
        end)()),
        V@3 = ((fun
          () ->
            {}
        end)()),
        ((('Test.Assert':assert())((V@3 =:= 1)))())
      end
  end).
main() -> 
  (fun
    () ->
      begin
        _@dollar__unused@0 = (('Snapshot.EffectRef':basicTest())()),
        (('Snapshot.EffectRef':onLetTest())())
      end
  end).

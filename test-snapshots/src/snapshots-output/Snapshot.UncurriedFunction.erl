-module('Snapshot.UncurriedFunction').
-compile(export_all).
test4a() -> 
  (fun
    (A@0) ->
      begin
        V@1 = (('Effect.Console':log())(A@0)),
        _@dollar__unused@2 = (V@1()),
        A@0
      end
  end).
test4b() -> 
  (fun
    () ->
      (('Snapshot.UncurriedFunction':test4a())(<<"test4b"/utf8>>))
  end).
test3a() -> 
  (fun
    (V@0, B@1) ->
      B@1
  end).
test3b() -> 
  (('Snapshot.UncurriedFunction':test3a())(1, 2)).
test2a() -> 
  (fun
    (A@0, V@1) ->
      A@0
  end).
test2b() -> 
  (('Snapshot.UncurriedFunction':test2a())(1, 2)).
test1a() -> 
  (('Data.Function.Uncurried':mkFn0())((fun
    (V@0) ->
      1
  end))).
test1b() -> 
  (('Data.Function.Uncurried':runFn0())(('Snapshot.UncurriedFunction':test1a()))).
main() -> 
  begin
    V@0 = (('Test.Assert':assert())((('Snapshot.UncurriedFunction':test1b()) =:= 1))),
    (fun
      () ->
        begin
          _@dollar__unused@1 = (V@0()),
          _@dollar__unused@2 = ((('Test.Assert':assert())((('Snapshot.UncurriedFunction':test2b()) =:= 1)))()),
          _@dollar__unused@3 = ((('Test.Assert':assert())((('Snapshot.UncurriedFunction':test3b()) =:= 2)))()),
          V@4 = (('Snapshot.UncurriedFunction':test4b())()),
          _@dollar__unused@5 = ((('Test.Assert':assert())((V@4 =:= <<"test4b"/utf8>>)))()),
          W@6 = (('Snapshot.UncurriedFunction':test4a())(<<"test4b"/utf8>>)),
          ((('Test.Assert':assert())((W@6 =:= <<"test4b"/utf8>>)))())
        end
    end)
  end.

-module(snapshot_nestedDo@ps).
-export([pure_/0, renamed/0, renamed0/0, renamed1/0, renamed2/0, do1/0, do0/0, bug26/0]).
pure_() ->
  (effect@ps:pureE()).
renamed() ->
  begin
    V = ((snapshot_nestedDo@ps:pure_())(unit)),
    (fun
      () ->
        begin
          A0 = (V()),
          A1 = (((snapshot_nestedDo@ps:pure_())(A0))()),
          _ = (((snapshot_nestedDo@ps:pure_())(A1))()),
          (((effect_console@ps:log())(<<"Oh no">>))())
        end
    end)
  end.
renamed0() ->
  begin
    V = ((snapshot_nestedDo@ps:pure_())(unit)),
    (fun
      () ->
        begin
          A0 = (V()),
          A = (((snapshot_nestedDo@ps:pure_())(A0))()),
          _ = (((snapshot_nestedDo@ps:pure_())(A))()),
          (((effect_console@ps:log())(<<"Oh no">>))())
        end
    end)
  end.
renamed1() ->
  begin
    V = ((snapshot_nestedDo@ps:pure_())(unit)),
    (fun
      () ->
        begin
          A = (V()),
          A@1 = (((snapshot_nestedDo@ps:pure_())(A))()),
          _ = (((snapshot_nestedDo@ps:pure_())(A@1))()),
          (((effect_console@ps:log())(<<"Oh no">>))())
        end
    end)
  end.
renamed2() ->
  begin
    V = ((snapshot_nestedDo@ps:pure_())(unit)),
    (fun
      () ->
        begin
          A = (V()),
          A1 = (((snapshot_nestedDo@ps:pure_())(A))()),
          _ = (((snapshot_nestedDo@ps:pure_())(A1))()),
          (((effect_console@ps:log())(<<"Oh no">>))())
        end
    end)
  end.
do1() ->
  begin
    V = ((snapshot_nestedDo@ps:pure_())(unit)),
    (fun
      () ->
        begin
          A = (V()),
          _ = (((snapshot_nestedDo@ps:pure_())(A))()),
          (((effect_console@ps:log())(<<"Oh no">>))())
        end
    end)
  end.
do0() ->
  begin
    V = ((snapshot_nestedDo@ps:pure_())(unit)),
    (fun
      () ->
        begin
          _ = (V()),
          (((effect_console@ps:log())(<<"Oh no">>))())
        end
    end)
  end.
bug26() ->
  begin
    V = ((snapshot_nestedDo@ps:pure_())(unit)),
    (fun
      () ->
        begin
          A = (V()),
          A@1 = (((snapshot_nestedDo@ps:pure_())(A))()),
          _ = (((snapshot_nestedDo@ps:pure_())(A@1))()),
          (((effect_console@ps:log())(<<"Oh no">>))())
        end
    end)
  end.

-module(snapshot_nestedDo@ps).
-export([pure_/0, renamed/0, renamed0/0, renamed1/0, renamed2/0, do1/0, do0/0, bug26/0]).
pure_() ->
  (effect@ps:pureE()).
renamed() ->
  begin
    V@0 = ((pure_())((data_unit@ps:unit()))),
    (fun
      () ->
        begin
          A0@1 = (V@0()),
          A1@2 = (((pure_())(A0@1))()),
          A2@3 = (((pure_())(A1@2))()),
          (((effect_console@ps:log())(<<"Oh no">>))())
        end
    end)
  end.
renamed0() ->
  begin
    V@0 = ((pure_())((data_unit@ps:unit()))),
    (fun
      () ->
        begin
          A0@1 = (V@0()),
          A@2 = (((pure_())(A0@1))()),
          A@3 = (((pure_())(A@2))()),
          (((effect_console@ps:log())(<<"Oh no">>))())
        end
    end)
  end.
renamed1() ->
  begin
    V@0 = ((pure_())((data_unit@ps:unit()))),
    (fun
      () ->
        begin
          A@1 = (V@0()),
          A@2 = (((pure_())(A@1))()),
          _@dollar__unused@3 = (((pure_())(A@2))()),
          (((effect_console@ps:log())(<<"Oh no">>))())
        end
    end)
  end.
renamed2() ->
  begin
    V@0 = ((pure_())((data_unit@ps:unit()))),
    (fun
      () ->
        begin
          A@1 = (V@0()),
          A1@2 = (((pure_())(A@1))()),
          A@3 = (((pure_())(A1@2))()),
          (((effect_console@ps:log())(<<"Oh no">>))())
        end
    end)
  end.
do1() ->
  begin
    V@0 = ((pure_())((data_unit@ps:unit()))),
    (fun
      () ->
        begin
          A@1 = (V@0()),
          A@2 = (((pure_())(A@1))()),
          (((effect_console@ps:log())(<<"Oh no">>))())
        end
    end)
  end.
do0() ->
  begin
    V@0 = ((pure_())((data_unit@ps:unit()))),
    (fun
      () ->
        begin
          A@1 = (V@0()),
          (((effect_console@ps:log())(<<"Oh no">>))())
        end
    end)
  end.
bug26() ->
  begin
    V@0 = ((pure_())((data_unit@ps:unit()))),
    (fun
      () ->
        begin
          A@1 = (V@0()),
          A@2 = (((pure_())(A@1))()),
          A@3 = (((pure_())(A@2))()),
          (((effect_console@ps:log())(<<"Oh no">>))())
        end
    end)
  end.

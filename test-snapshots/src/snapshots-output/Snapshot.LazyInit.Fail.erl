% Snapshot.LazyInit.Fail
-module(snapshot_lazyInit_fail@ps).
-export([main/0, force/0, force/1, x/0, x/1]).
-compile(no_auto_import).
main() ->
  fun
    () ->
      unit
  end.

force() ->
  fun force/1.

force(F) ->
  F(unit).

x() ->
  fun x/1.

x(_) ->
  begin
    SelfOwn =
      fun
        SelfOwn () ->
          #{ a => 1, b => erlang:map_get(a, SelfOwn()) }
      end,
    SelfOwn()
  end.


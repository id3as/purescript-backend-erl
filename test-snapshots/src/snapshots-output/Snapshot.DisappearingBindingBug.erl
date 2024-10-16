% Snapshot.DisappearingBindingBug
-module(snapshot_disappearingBindingBug@ps).
-export([thing/0, thing/1]).
-compile(no_auto_import).
thing() ->
  fun thing/1.

thing(Opts) ->
  if
    erlang:map_get(prefix, Opts) ->
      [<<"prefix">>];
    true ->
      [<<"prefix">>]
  end
    ++ if
      erlang:map_get(secondBit, Opts) ->
        [<<"secondbit">>];
      true ->
        [<<"nosecondbit">>]
    end.


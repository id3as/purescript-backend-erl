% Snapshot.Scoping
-module(snapshot_scoping@ps).
-export([noInline/0, noInline/1, inline/0, inline/1, ex/0, ex/1]).
-compile(no_auto_import).
noInline() ->
  fun noInline/1.

noInline(A) ->
  A.

inline() ->
  fun inline/1.

inline(N) ->
  begin
    A = noInline(N),
    A + A
  end.

ex() ->
  fun ex/1.

ex(N) ->
  begin
    A@1 =
      if
        N =:= 0 ->
          begin
            A = noInline(N),
            A + A
          end;
        true ->
          2
      end,
    if
      A@1 =:= 2 ->
        0;
      true ->
        A@1
    end
  end.


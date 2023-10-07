-module(snapshot_scoping@ps).
-export([noInline/0, noInline/1, inline/0, inline/1, ex/0, ex/1]).
-compile(no_auto_import).
noInline() ->
  fun
    (A@Local) ->
      noInline(A@Local)
  end.

noInline(A) ->
  A.

inline() ->
  fun
    (N@Local) ->
      inline(N@Local)
  end.

inline(N) ->
  begin
    A = (noInline(N)),
    A + A
  end.

ex() ->
  fun
    (N@Local) ->
      ex(N@Local)
  end.

ex(N) ->
  begin
    A@1 = if
      N =:= 0 ->
        begin
          A = (noInline(N)),
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


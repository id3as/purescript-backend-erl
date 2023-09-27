-module(snapshot_scoping@ps).
-export([noInline/0, noInline/1, inline/0, inline/1, ex/0, ex/1]).
-compile(no_auto_import).
noInline() ->
  (fun
    (A@Local) ->
      (noInline(A@Local))
  end).
noInline(A) ->
  A.
inline() ->
  (fun
    (N@Local) ->
      (inline(N@Local))
  end).
inline(N) ->
  begin
    A = ((snapshot_scoping@ps:noInline())(N)),
    (A + A)
  end.
ex() ->
  (fun
    (N@Local) ->
      (ex(N@Local))
  end).
ex(N) ->
  begin
    A@1 = case (N =:= 0) of
      true ->
        begin
          A = ((snapshot_scoping@ps:noInline())(N)),
          (A + A)
        end;
      _ ->
        2
    end,
    case (A@1 =:= 2) of
      true ->
        0;
      _ ->
        A@1
    end
  end.

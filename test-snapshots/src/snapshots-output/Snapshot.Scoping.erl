-module(snapshot_scoping@ps).
-export([noInline/0, noInline/1, inline/0, inline/1, ex/0, ex/1]).
noInline() ->
  (fun
    (V@0) ->
      (noInline(V@0))
  end).
noInline(A) ->
  A.
inline() ->
  (fun
    (V@0) ->
      (inline(V@0))
  end).
inline(N) ->
  begin
    A = ((snapshot_scoping@ps:noInline())(N)),
    (A + A)
  end.
ex() ->
  (fun
    (V@0) ->
      (ex(V@0))
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

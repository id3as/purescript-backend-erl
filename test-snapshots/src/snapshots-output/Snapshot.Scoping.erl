-module(snapshot_scoping@ps).
-export([noInline/0, inline/0, ex/0]).
noInline() ->
  (fun
    (A) ->
      A
  end).
inline() ->
  (fun
    (N) ->
      begin
        A = ((snapshot_scoping@ps:noInline())(N)),
        (A + A)
      end
  end).
ex() ->
  (fun
    (N) ->
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
      end
  end).

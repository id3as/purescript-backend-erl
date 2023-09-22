-module(snapshot_scoping@ps).
-export([noInline/0, inline/0, ex/0]).
noInline() ->
  (fun
    (A@0) ->
      A@0
  end).
inline() ->
  (fun
    (N@0) ->
      begin
        A@1 = ((snapshot_scoping@ps:noInline())(N@0)),
        (A@1 + A@1)
      end
  end).
ex() ->
  (fun
    (N@0) ->
      begin
        A@1 = case (N@0 =:= 0) of
          true ->
            begin
              A@1 = ((snapshot_scoping@ps:noInline())(N@0)),
              (A@1 + A@1)
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

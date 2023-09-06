-module(snapshot_let).
letChain() -> 
  (fun
    (X@0) -> 
      begin
        A@1 = (X@0 + X@0),
        begin
          B@2 = (A@1 + A@1),
          begin
            C@3 = (B@2 + B@2),
            (((A@1 + B@2) + C@3) + (C@3 * C@3))
          end
        end
      end
  end).

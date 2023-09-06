-module(snapshot_comparison).
stringComparison() -> 
  (fun
    (X@0) -> 
      (fun
        (Y@1) -> 
          [(X@0 =:= Y@1),(X@0 =/= Y@1),(X@0 < Y@1),(X@0 =< Y@1),(X@0 >= Y@1),(X@0 > Y@1)]
      end)
  end).
numberComparison() -> 
  (fun
    (X@0) -> 
      (fun
        (Y@1) -> 
          [(X@0 =:= Y@1),(X@0 =/= Y@1),(X@0 < Y@1),(X@0 =< Y@1),(X@0 >= Y@1),(X@0 > Y@1)]
      end)
  end).
integerComparison() -> 
  (fun
    (X@0) -> 
      (fun
        (Y@1) -> 
          [(X@0 =:= Y@1),(X@0 =/= Y@1),(X@0 < Y@1),(X@0 =< Y@1),(X@0 >= Y@1),(X@0 > Y@1)]
      end)
  end).
charComparison() -> 
  (fun
    (X@0) -> 
      (fun
        (Y@1) -> 
          [(X@0 =:= Y@1),(X@0 =/= Y@1),(X@0 < Y@1),(X@0 =< Y@1),(X@0 >= Y@1),(X@0 > Y@1)]
      end)
  end).
booleanComparison() -> 
  (fun
    (X@0) -> 
      (fun
        (Y@1) -> 
          [(X@0 =:= Y@1),(X@0 =/= Y@1),(X@0 < Y@1),(X@0 =< Y@1),(X@0 >= Y@1),(X@0 > Y@1)]
      end)
  end).

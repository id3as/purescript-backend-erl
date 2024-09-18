-module(test).
-compile(export_all).

atom() -> (fun (X) -> atom end)(1).
list() -> (fun (X) -> [1,2,3,4,5] end)(2).
id(X) -> (fun (Y) -> X end)(3).
bif(X) -> (fun (Y) -> erlang:atom_to_binary(X) end)(4).
fgn(X) -> (fun (Y) -> foreign:redacted(X) end)(5).

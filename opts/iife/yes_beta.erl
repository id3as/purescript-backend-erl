-module(test).
-compile(export_all).

atom() -> (fun (X) -> {X,atom} end)(1).
list() -> (fun (X) -> [1,X,3,4,5] end)(2).
add(X) -> (fun (Y) -> X + Y end)(3).
bif(X) -> (fun (Y) -> begin XX = erlang:atom_to_binary(X), << XX, Y >> end end)(<<"Y">>).
fgn(X) -> (fun (Y) -> foreign:redacted(X, Y) end)(5).

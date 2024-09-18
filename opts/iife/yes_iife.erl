-module(test).
-compile(export_all).

atom() -> (fun () -> atom end)().
list() -> (fun () -> [1,2,3,4,5] end)().
id(X) -> (fun () -> X end)().
bif(X) -> (fun () -> erlang:atom_to_binary(X) end)().
fgn(X) -> (fun () -> foreign:redacted(X) end)().

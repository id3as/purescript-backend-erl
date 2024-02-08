-module(test).
-compile(export_all).

atom() -> (fun () -> atom end)().
list() -> (fun () -> [1,2,3,4,5] end)().
id(x) -> (fun () -> x end)().
bif(x) -> (fun () -> erlang:atom_to_binary(x) end)().
fgn(x) -> (fun () -> foreign:redacted(x) end)().

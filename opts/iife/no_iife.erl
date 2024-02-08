-module(test).
-compile(export_all).

atom() -> atom.
list() -> [1,2,3,4,5].
id(x) -> x.
bif(x) -> erlang:atom_to_binary(x).
fgn(x) -> foreign:redacted(x).

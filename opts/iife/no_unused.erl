-module(test).
-compile(export_all).

atom() -> atom.
list() -> [1,2,3,4,5].
id(X) -> X.
bif(X) -> erlang:atom_to_binary(X).
fgn(X) -> foreign:redacted(X).

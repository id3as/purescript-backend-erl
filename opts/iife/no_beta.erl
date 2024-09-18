-module(test).
-compile(export_all).

atom() -> {1, atom}.
list() -> [1,2,3,4,5].
add(X) -> X + 3.
bif(X) -> begin XX = erlang:atom_to_binary(X), Y = << "Y" >>, << XX, Y >> end.
fgn(X) -> foreign:redacted(X, 5).

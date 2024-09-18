-module(test).
-compile(export_all).

% yes: compiled as a literal
test1() -> #{ x => 1, y => 2 }.

% yes: compiled as a literal
test2() -> #{ x => 1, y => [] }.

% no: compiled as a closure and map allocation
test3() -> #{ x => 1, y => [], z => fun test1/0 }.

% no: compiled as a closure and map allocation
test4() -> #{ x => 1, y => [], z => fun (X) -> io:format(X) end }.

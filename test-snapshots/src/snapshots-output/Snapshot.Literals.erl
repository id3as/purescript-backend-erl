-module(snapshot_literals@ps).
-compile(export_all).
string() ->
  <<"string">>.
record2() ->
  #{foo => <<"bar">>}.
record() ->
  #{}.
number() ->
  2.0.
int() ->
  1.
char() ->
  $a.
boolean2() ->
  false.
boolean1() ->
  true.
array2() ->
  (array:from_list([1,2,3])).
array() ->
  (array:from_list([])).

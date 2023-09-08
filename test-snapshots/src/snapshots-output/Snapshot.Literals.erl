-module(snapshot_literals@ps).
-export([string/0, record2/0, record/0, number/0, int/0, char/0, boolean2/0, boolean1/0, array2/0, array/0]).
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

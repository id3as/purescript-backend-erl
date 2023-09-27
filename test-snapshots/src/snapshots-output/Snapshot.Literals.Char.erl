-module(snapshot_literals_char@ps).
-export([vtab/0, tab/0, space/0, singleQuote/0, return/0, page/0, null/0, newline/0, ls/0, latinExtendedB/0, latinExtendedA/0, latin1Supplement/0, lastPrintableChar/0, forallChar/0, firstPrintableChar/0, escape/0, doubleQuote/0, delete/0, closeToTop/0, biggestChar/0, backspace/0, alarm/0]).
-compile(no_auto_import).
vtab() ->
  $\v.
tab() ->
  $\t.
space() ->
  $ .
singleQuote() ->
  $\'.
return() ->
  $\r.
page() ->
  $\f.
null() ->
  $\x{0}.
newline() ->
  $\n.
ls() ->
  $\x{8232}.
latinExtendedB() ->
  $\x{384}.
latinExtendedA() ->
  $\x{257}.
latin1Supplement() ->
  $\x{177}.
lastPrintableChar() ->
  $~.
forallChar() ->
  $\x{8704}.
firstPrintableChar() ->
  $!.
escape() ->
  $\e.
doubleQuote() ->
  $\".
delete() ->
  $\d.
closeToTop() ->
  $\x{65049}.
biggestChar() ->
  $\x{65535}.
backspace() ->
  $\b.
alarm() ->
  $\x{7}.

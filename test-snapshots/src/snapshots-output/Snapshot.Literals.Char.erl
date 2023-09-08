-module(snapshot_literals_char@ps).
-compile(export_all).
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

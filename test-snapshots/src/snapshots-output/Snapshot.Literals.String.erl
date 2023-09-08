-module(snapshot_literals_string@ps).
-compile(export_all).
unicode7() ->
  <<"Foo \b\b\b\b Bar">>.
unicode6() ->
  <<"2342✓"/utf8>>.
unicode5() ->
  <<"Foo ✓ Bar ✓ \n Baz ✓"/utf8>>.
unicode4() ->
  <<"✓"/utf8>>.
unicode3() ->
  <<"✓"/utf8>>.
unicode2() ->
  <<"􏿿"/utf8>>.
unicode1() ->
  <<"\x{0}">>.
escape8() ->
  <<"\'">>.
escape7() ->
  <<"\"">>.
escape6() ->
  <<"\\">>.
escape5() ->
  <<"\r\n">>.
escape4() ->
  <<"\r">>.
escape3() ->
  <<"\r">>.
escape2() ->
  <<"\n">>.
escape1() ->
  <<"\t">>.
block5() ->
  <<"foo\nbar\nbaz\n">>.
block4() ->
  <<"foo\nbar\nbaz">>.
block3() ->
  <<"foo\nbar">>.
block2() ->
  <<"\nfoo\nbar\n">>.
block1() ->
  <<"block">>.

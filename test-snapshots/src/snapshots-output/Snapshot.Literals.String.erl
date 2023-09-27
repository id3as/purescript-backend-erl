-module(snapshot_literals_string@ps).
-export([unicode7/0, unicode6/0, unicode5/0, unicode4/0, unicode3/0, unicode2/0, unicode1/0, escape8/0, escape7/0, escape6/0, escape5/0, escape4/0, escape3/0, escape2/0, escape1/0, block5/0, block4/0, block3/0, block2/0, block1/0]).
-compile(no_auto_import).
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

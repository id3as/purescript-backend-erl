-module(snapshot_literals_string).
-compile(export_all).
unicode7() -> 
  <<"Foo \b\b\b\b Bar"/utf8>>.
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
  <<"\x{0}"/utf8>>.
escape8() -> 
  <<"\'"/utf8>>.
escape7() -> 
  <<"\""/utf8>>.
escape6() -> 
  <<"\\"/utf8>>.
escape5() -> 
  <<"\r\n"/utf8>>.
escape4() -> 
  <<"\r"/utf8>>.
escape3() -> 
  <<"\r"/utf8>>.
escape2() -> 
  <<"\n"/utf8>>.
escape1() -> 
  <<"\t"/utf8>>.
block5() -> 
  <<"foo\nbar\nbaz\n"/utf8>>.
block4() -> 
  <<"foo\nbar\nbaz"/utf8>>.
block3() -> 
  <<"foo\nbar"/utf8>>.
block2() -> 
  <<"\nfoo\nbar\n"/utf8>>.
block1() -> 
  <<"block"/utf8>>.

-module(snapshot_literals_string).
unicode7() -> 
  <<"Foo  Bar">>.
unicode6() -> 
  <<"2342✓">>.
unicode5() -> 
  <<"Foo ✓ Bar ✓ 
 Baz ✓">>.
unicode4() -> 
  <<"✓">>.
unicode3() -> 
  <<"✓">>.
unicode2() -> 
  <<"􏿿">>.
unicode1() -> 
  <<" ">>.
escape8() -> 
  <<"'">>.
escape7() -> 
  <<""">>.
escape6() -> 
  <<"\">>.
escape5() -> 
  <<"
">>.
escape4() -> 
  <<"">>.
escape3() -> 
  <<"">>.
escape2() -> 
  <<"
">>.
escape1() -> 
  <<"	">>.
block5() -> 
  <<"foo
bar
baz
">>.
block4() -> 
  <<"foo
bar
baz">>.
block3() -> 
  <<"foo
bar">>.
block2() -> 
  <<"
foo
bar
">>.
block1() -> 
  <<"block">>.

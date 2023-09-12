-- @expected #{test1 => true, test2 => 2, test3 => 3, test4 => 4, test5 => 5}
module Snapshot.ConstructorAccessor where

import Prelude

import Partial.Unsafe (unsafePartial)

data Foo = Foo Int Int Int

-- A constructor that is partially applied to its arguments.
x :: Int -> Int -> Foo
x = Foo 1

y :: Int -> Foo
y = Foo 1 1

z :: Foo
z = Foo 1 1 1

-- more pattern matching tests
data NoArgs = NoArgs

test1 :: NoArgs -> Boolean
test1 = case _ of
  NoArgs -> true

data HasArgs = HasArgs Int Int Int

test2 :: HasArgs -> Int
test2 = case _ of
  HasArgs i1 _ _ -> i1

test3 :: HasArgs -> Int
test3 = case _ of
  HasArgs i1 i2 i3
    | i1 < i3 -> i1
    | otherwise -> i2

data SumWithArgs
  = First Int
  | Last Int

test4 :: SumWithArgs -> Int
test4 = case _ of
  First i -> i
  Last i -> i

test5 :: Partial => SumWithArgs -> Int
test5 = case _ of
  First i -> i

result ::
  { test1 :: Boolean
  , test2 :: Int
  , test3 :: Int
  , test4 :: Int
  , test5 :: Int
  }
result = unsafePartial
  { test1: test1 NoArgs
  , test2: test2 (HasArgs 2 1 0)
  , test3: test3 (HasArgs 5 3 1)
  , test4: test4 (Last 4)
  , test5: test5 (First 5)
  }

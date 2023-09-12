-- @inline Snapshot.ConstructorAccessor.dontInlineMe never
-- @expected #{f0=>0,f1=>1,f2=>2,f3=>3,g0=>0,g1=>1,g2=>2,g3=>3,ittf=>false,ifft=>true,iftf=>false,itft=>true,h=>3.14159}
module Snapshot.Branch where

import Partial.Unsafe (unsafePartial)

-- import Partial.Unsafe (unsafePartial)

f :: Boolean -> Boolean -> Boolean -> Int
f x y z = if x then if y then if z then 0 else 1 else 2 else 3

g :: Int -> Int
g = case _ of
  0 -> 1
  1 -> 2
  2 -> 3
  _ -> 0

h :: Number -> Number
h = unsafePartial case _ of
  3.14 -> 3.14159

i :: Boolean -> Boolean -> Boolean
i = case _, _ of
  true, true -> false
  false, false -> true
  false, true -> false
  true, false -> true

dontInlineMe :: forall a. a -> a
dontInlineMe a = a

result ::
  { f0 :: Int
  , f1 :: Int
  , f2 :: Int
  , f3 :: Int
  , g0 :: Int
  , g1 :: Int
  , g2 :: Int
  , g3 :: Int
  , h :: Number
  , ifft :: Boolean
  , iftf :: Boolean
  , itft :: Boolean
  , ittf :: Boolean
  }
result =
  { f0: f true true true
  , f1: f true true false
  , f2: f true false true
  , f3: f false false false
  , g0: g 5
  , g1: g 0
  , g2: g 1
  , g3: g 2
  , ittf: i true true
  , ifft: i false false
  , iftf: i false true
  , itft: i true false
  , h: unsafePartial h 3.14
  }

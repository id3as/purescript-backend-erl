-- @inline Snapshot.Let.GoArity.foldr never
-- @expected 10
module Snapshot.Let.GoArity where

import Prelude

import Data.Foldable (foldl)
import Data.List (List(..), (:))

foldr :: forall a b. (a -> b -> b) -> b -> List a -> b
foldr f b = foldl (flip f) b <<< rev
  where
  rev = go Nil
    -- Generate a recursive function of arity 2 for `go`, but call it curried
    --
    -- `foldl` also gets inlined to a recursive function named `go`,
    -- so this tests for shadowing there too
    where
    go acc Nil = acc
    go acc (x : xs) = go (x : acc) xs

result :: Int
result = foldr (+) 1 (2 : 3 : 4 : Nil)

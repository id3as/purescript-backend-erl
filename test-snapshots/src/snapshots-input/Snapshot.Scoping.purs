-- @inline Snapshot.Scoping.noInline never
module Snapshot.Scoping where

import Data.Semiring ((+))

noInline :: forall a. a -> a
noInline a = a

inline :: Int -> Int
inline n =
  -- important: needs to have the same name as below
  -- (or they both need to be binders without names)
  let a = noInline n
  in a + a

ex :: Int -> Int
ex n =
  let
    a = case n of
      0 -> inline n
      _ -> 2
  in case a of
    2 -> 0
    _ -> a

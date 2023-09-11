-- @expected {tuple, "fib", 8}
module Snapshot.Let.NotAbs where

import Data.Functor (map)
import Data.Ord ((<))
import Data.Ring ((+), (-))
import Data.Tuple (Tuple(..), snd)

result :: Tuple String Int
result =
  let
    fibAnd :: Tuple String (Int -> Int)
    fibAnd = Tuple "fib" \n ->
      if n < 2 then n else
        snd fibAnd (n - 1) + case fibAnd of
          Tuple _ f -> f (n - 2)
  in map (\f -> f 6) fibAnd

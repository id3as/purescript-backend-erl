-- @expected 8
module Snapshot.Let.Fib where

import Data.Ord ((<))
import Data.Ring ((+), (-))

result :: Int
result =
  let
    fib :: Int -> Int
    fib n | n < 2 = n
    fib n = fib (n - 1) + fib (n - 2)
  in fib 6

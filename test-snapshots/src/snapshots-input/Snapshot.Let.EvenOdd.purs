-- @expected {tuple, false, true}
module Snapshot.Let.EvenOdd where

import Data.Eq ((==))
import Data.Ring ((-))
import Data.Tuple (Tuple(..))

result :: Tuple Boolean Boolean
result =
  let
    isEven :: Int -> Boolean
    isEven n | n == 0 = true
    isEven n = isOdd (n - 1)
    isOdd :: Int -> Boolean
    isOdd n | n == 0 = false
    isOdd n = isEven (n - 1)
  in Tuple (isEven 5) (isOdd 5)

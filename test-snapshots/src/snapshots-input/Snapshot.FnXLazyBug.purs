-- @inline Snapshot.FnXLazyBug.zipWith4 never
-- @expected [15, 26]
module Snapshot.FnXLazyBug where

import Prelude
import Data.Function.Uncurried (mkFn5, runFn5)
import Data.Maybe (Maybe(..))
import Erl.Data.List (List, nil, reverse, uncons, (:))

zipWith4 :: forall a b c d e. (a -> b -> c -> d -> e) -> List a -> List b -> List c -> List d -> List e
zipWith4 f as bs cs ds = runFn5 go nil as bs cs ds
  where
  go = mkFn5 \acc as bs cs ds -> case uncons as, uncons bs, uncons cs, uncons ds of
    Just az, Just bz, Just cz, Just dz ->
      runFn5 go (f az.head bz.head cz.head dz.head : acc) az.tail bz.tail cz.tail dz.tail
    _, _, _, _ -> reverse acc

result :: List Int
result = zipWith4 (\a b c d -> a * b + c * d)
  (1 : 2 : 9 : nil)
  (0 : 1 : nil)
  (3 : 4 : nil)
  (5 : 6 : 7 : 8 : nil)

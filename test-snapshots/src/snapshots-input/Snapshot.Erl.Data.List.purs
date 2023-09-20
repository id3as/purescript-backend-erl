-- @expected #{hd=>{just,1},concat=>[1,2,3,4,5,6]}
module Snapshot.Erl.Data.List where

import Prelude

import Data.Maybe (Maybe)
import Erl.Data.List (nil, (:))
import Erl.Data.List as L

lit1 :: L.List Int
lit1 = 1 : 2 : 3 : nil

hd :: Maybe Int
hd = L.head lit1

lit2 :: L.List Int
lit2 = L.fromFoldable [ 4, 5, 6 ]

concat :: L.List Int
concat = lit1 <> lit2

n :: L.List Int
n = nil

result :: Record _
result =
  { hd
  , concat
  }

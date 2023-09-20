-- -- @inline Snapshot.Erl.Data.List.lit1 always
-- -- @inline Snapshot.Erl.Data.List.lit2 always
-- @inline Data.Foldable.foldrArray arity=3
-- -- ^ this is to undo CSE
-- @expected #{hd=>{just,1},concatNeut=>[1,2,3,4,5,6],concatLR=>[1,2,3,4]}
module Snapshot.Erl.Data.List where

import Prelude

import Data.Maybe (Maybe)
import Erl.Data.List (nil, (:))
import Erl.Data.List as L

lit1 :: L.List Int
lit1 = 1 : 2 : 3 : nil

hd :: Maybe Int
hd = L.head lit1

lit0 :: L.List Int
lit0 = L.fromFoldable []

lit2 :: L.List Int
lit2 = L.fromFoldable [ 4, 5, 6 ]

concatNeut :: L.List Int
concatNeut = lit1 <> lit2

concat :: L.List Int
concat = (1 : 2 : 3 : nil) <> L.fromFoldable [ 4, 5, 6 ]

concatSimple :: L.List Int -> L.List Int -> L.List Int
concatSimple l r = (1 : 2 : l) <> (3 : r)

concatL :: L.List Int -> L.List Int
concatL l = (1 : 2 : nil) <> (3 : l)

concatLR :: L.List Int
concatLR = (1 : 2 : nil) <> (3 : 4 : nil)

uncons0 l = L.uncons (l <> l)
uncons1 = L.uncons (1 : 2 : 3 : nil)
uncons2 = L.uncons nil
unconsA0 = L.uncons (L.fromFoldable [])
unconsA1 = L.uncons (L.fromFoldable [ 1 ])

n :: L.List Int
n = nil

result :: Record _
result =
  { hd
  , concatNeut
  , concatLR
  }

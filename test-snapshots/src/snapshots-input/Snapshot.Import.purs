-- @expected 43
module Snapshot.Import where

import Snapshot.Import.Impl as I

fortyThree :: Int
fortyThree = I.addImpl 21 22

result = fortyThree

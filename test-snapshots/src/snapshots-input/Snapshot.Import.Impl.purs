-- @expected 42
module Snapshot.Import.Impl where

foreign import addImpl :: Int -> Int -> Int

fortyTwo :: Int
fortyTwo = addImpl 21 21

result = fortyTwo

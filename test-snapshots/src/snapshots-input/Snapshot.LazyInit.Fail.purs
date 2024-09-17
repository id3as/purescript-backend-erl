module Snapshot.LazyInit.Fail where

import Prelude

import Effect (Effect)

force :: forall b. (Unit -> b) -> b
force f = f unit

-- Currently this leads to infinite recursion instead of throwing an error about
-- accessing an uninitialized variable
x :: Int -> _
x _i = do
  let
    selfOwn = { a: 1, b: force \_ -> selfOwn.a }
  selfOwn

main :: Effect _
main = do
  -- pure (x 5)
  pure unit

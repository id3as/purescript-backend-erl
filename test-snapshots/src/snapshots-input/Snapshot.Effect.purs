-- @inline Snapshot.Effect.don'tInlineMeMe never
module Snapshot.Effect where

import Prelude

import Effect (Effect)

don'tInlineMeMe :: forall a. a -> Effect Unit
don'tInlineMeMe _ = pure unit

lastComponentIsRun :: Effect Unit
lastComponentIsRun = do
  don'tInlineMeMe "a"
  don'tInlineMeMe "b"
  don'tInlineMeMe "c"

lastPureIsUnwrapped :: Effect Unit
lastPureIsUnwrapped = do
  value <- don'tInlineMeMe "a"
  don'tInlineMeMe "b"
  pure value

main :: Effect Unit
main = do
  lastComponentIsRun
  lastPureIsUnwrapped

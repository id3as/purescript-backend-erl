-- @inline Snapshot.Effect.don'tInlineMeMe never
-- @inline Snapshot.Effect.caseInChain never
-- @inline Snapshot.Effect.doWhen never
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

caseInChain :: Int -> Effect Int
caseInChain i = do
  _ <- pure i
  v <- case i + 4 of
    0 -> pure 0
    j -> do
      don'tInlineMeMe "x"
      pure (j - 4)
  don'tInlineMeMe "y"
  case compare i 8 of
    EQ -> pure unit
    LT -> don'tInlineMeMe "LT"
    GT -> don'tInlineMeMe "GT"
  pure v

doWhen :: Boolean -> Effect Unit
doWhen b = do
  when b do
    don'tInlineMeMe "b"
  when (not b) do
    don'tInlineMeMe "not b"

main :: Effect Unit
main = do
  _ <- caseInChain 3
  doWhen true
  doWhen false
  lastComponentIsRun
  lastPureIsUnwrapped

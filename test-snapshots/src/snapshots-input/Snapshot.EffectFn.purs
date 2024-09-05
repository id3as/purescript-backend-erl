-- @inline Snapshot.EffectFn.callY never
module Snapshot.EffectFn where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Uncurried (EffectFn2, mkEffectFn2, runEffectFn2)

x :: forall a2 b3. String -> EffectFn2 a2 b3 Unit
x a = mkEffectFn2 \_ _ -> do
  log a

y :: forall a9 b10. EffectFn2 a9 b10 Unit
y = mkEffectFn2 \a b -> runEffectFn2 (x "Hi") a b

z :: forall a9 b10. EffectFn2 a9 b10 Unit
z = mkEffectFn2 \a b -> runEffectFn2 (x "Hi") a b

callY :: EffectFn2 String String Unit -> Effect Unit
callY f = do
  runEffectFn2 f "0" "1"
  runEffectFn2 f "2" "3"

main :: Effect Unit
main = do
  runEffectFn2 y unit unit
  callY y

main1 :: Effect Unit
main1 = do
  runEffectFn2 y unit unit
  pure unit

main2 :: Effect Unit
main2 = do
  runEffectFn2 y unit unit
  callY z

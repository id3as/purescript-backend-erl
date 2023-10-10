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

main :: Effect Unit
main = runEffectFn2 y unit unit

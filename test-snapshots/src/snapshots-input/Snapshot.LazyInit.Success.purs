module Snapshot.LazyInit.Success where

import Prelude

import Effect (Effect)

complicatedIdentity :: forall a. Int -> a -> a
complicatedIdentity _definitelyUsed {- to force local bindings -} = h
  where
  -- This highly contrived function tests that escalating force is caught and
  -- doesn't cause an infinite loop during compilation. ("Escalating force"
  -- means that invoking `f` with two argument leads to `f` being invoked with
  -- three arguments, and so on.)

  -- If the escalating loop in `f` isn't taken into account, `h` might be
  -- initialized before `g`, which will lead to a run-time error. The intended
  -- behavior is to lazily initialize `g` and `h` together, and let the fact
  -- that at run time `g` never actually dereferences `h` resolve the
  -- initialization ordering.

  f :: forall a. Int -> { tick :: a -> a, tock :: a -> a }
  f n = { tick: if n <= 0 then identity else (f (n - 1)).tock identity, tock: \a -> g n a }

  g :: forall a. Int -> a -> a
  g = (\bit -> if bit then \n -> (f n).tick else const h) true

  h :: forall a. a -> a
  h = (\n -> (f n).tick) 10

main :: Effect Unit
main = do
  pure (complicatedIdentity 5 unit)

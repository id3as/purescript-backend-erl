-- @inline Snapshot.PatternGuardDemand.scanFlip never
-- @inline Snapshot.PatternGuardDemand.processE never
-- | Regression test: constructor refinements must not escape function
-- | boundaries in the demand analysis (Convert.After). `processE` has a
-- | pattern-guarded clause, which forces the whole function into a single
-- | erlang clause; the fallthrough continuation is a `fun` whose body knows
-- | `pipeline` is `{just, _}` (its sibling branch is an unreachable match
-- | failure). If that refinement leaks to the definition site, the function
-- | HEAD becomes `#{ pipeline := {just, _} }` and the `Nothing` clauses below
-- | are unreachable: `main`'s first call dies with function_clause.
module Snapshot.PatternGuardDemand where

import Prelude

import Data.Foldable (any)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref as Ref

type Compiled = { gpu :: Boolean, refs :: Int }

type State =
  { pipeline :: Maybe Compiled
  , meta :: Maybe Int
  , order :: Array String
  , pending :: Int
  , held :: Int
  , metrics :: Int
  , dropping :: Int
  }

type Payload = { source :: String, payload :: { p :: Int, pts :: Int } }

-- Stand-in for a rare-event detector (scan-structure flip in the original).
scanFlip :: State -> String -> Maybe Int
scanFlip s _ = if s.metrics > 10 then Just 1 else Nothing

processE :: Ref.Ref Int -> State -> Payload -> Effect Int
processE _ { pipeline: Nothing, meta: Nothing } _ = pure 0
processE _ { pipeline: Nothing, meta: Just m } { payload: { p } } = pure (m + p)
processE _ state@{ pipeline: Just _ } { source }
  | not (any (_ == source) state.order) = pure 1
processE ref state@{ pipeline: Just _ } tp@{ source, payload: { p } }
  | Just x <- scanFlip state source = do
      Ref.modify_ (_ + state.pending) ref
      Ref.modify_ (_ + state.held) ref
      Ref.modify_ (_ + state.metrics) ref
      r <- processE ref (state { pipeline = Nothing, metrics = 0 }) tp
      pure (r + x + p)
processE ref { pipeline: Just { gpu, refs }, pending, metrics, dropping } { payload: { p, pts } } = do
  buf <- if gpu then pure refs else pure 0
  Ref.modify_ (_ + (metrics + pending)) ref
  Ref.modify_ (_ + dropping) ref
  pure (buf + p + pts)

baseState :: State
baseState =
  { pipeline: Nothing
  , meta: Nothing
  , order: [ "s" ]
  , pending: 1
  , held: 2
  , metrics: 3
  , dropping: 4
  }

basePayload :: Payload
basePayload = { source: "s", payload: { p: 10, pts: 100 } }

assertEq :: String -> Int -> Int -> Effect Unit
assertEq label x y =
  when (x /= y) do
    throw (label <> ": got " <> show x <> ", expected " <> show y)

main :: Effect Unit
main = do
  ref <- Ref.new 0
  -- No compiled pipeline yet: these MUST hit the Nothing clauses (the bug
  -- makes the function head require {just, _}, crashing function_clause here).
  a <- processE ref baseState basePayload
  assertEq "nothing/nothing" a 0
  b <- processE ref (baseState { meta = Just 5 }) basePayload
  assertEq "nothing/just" b 15
  -- Unknown source: boolean-guard clause.
  c <- processE ref (baseState { pipeline = Just { gpu: false, refs: 7 } }) (basePayload { source = "other" })
  assertEq "unknown source" c 1
  -- Steady state: final clause (gpu branch exercises the refs field).
  d <- processE ref (baseState { pipeline = Just { gpu: true, refs: 7 } }) basePayload
  assertEq "steady gpu" d 117
  -- Pattern-guard clause fires, recompiles (recurses into nothing/nothing).
  e <- processE ref (baseState { pipeline = Just { gpu: false, refs: 7 }, metrics = 11 }) basePayload
  assertEq "scan flip" e 11

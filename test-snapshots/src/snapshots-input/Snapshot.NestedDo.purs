-- @inline Snapshot.NestedDo.pure_ never
module Snapshot.NestedDo where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

pure_ :: forall a. a -> Effect a
pure_ = pure

bug26 :: Effect Unit
bug26 = do
  a <- do
    a <- do
      a <- pure_ unit
      pure_ a
    pure_ a
  log "Oh no"

renamed :: Effect Unit
renamed = do
  a2 <- do
    a1 <- do
      a0 <- pure_ unit
      pure_ a0
    pure_ a1
  log "Oh no"

renamed0 :: Effect Unit
renamed0 = do
  a <- do
    a <- do
      a0 <- pure_ unit
      pure_ a0
    pure_ a
  log "Oh no"

renamed1 :: Effect Unit
renamed1 = do
  do
    a <- do
      a <- pure_ unit
      pure_ a
    pure_ a
  log "Oh no"

renamed2 :: Effect Unit
renamed2 = do
  a <- do
    a1 <- do
      a <- pure_ unit
      pure_ a
    pure_ a1
  log "Oh no"


do0 :: Effect Unit
do0 = do
  a <- pure_ unit
  log "Oh no"

do1 :: Effect Unit
do1 = do
  a <- do
    a <- pure_ unit
    pure_ a
  log "Oh no"


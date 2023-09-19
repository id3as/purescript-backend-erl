-- @inline Control.Monad.State.Trans.bindStateT(..).bind arity=2
-- @inline Control.Monad.State.Trans.applyStateT(..).apply arity=2
-- @inline Control.Monad.State.Trans.applicativeStateT(..).pure arity=1
-- @inline Control.Monad.State.Trans.functorStateT(..).map arity=2
-- @inline Control.Monad.State.Trans.monadStateStateT(..).state arity=1
-- @inline Control.Monad.State.Trans.monadTransStateT(..).lift(..) always
-- @inline Control.Monad.State.Trans.monadEffectState(..).liftEffect arity=1
module Snapshot.State.Log where

import Prelude

import Control.Monad.State as S
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Console as Console

exE :: S.StateT Int Effect { a :: Int, b :: Int }
exE = do
  a <- freshE
  S.StateT \s -> Tuple <$> Console.log (show a) <@> s
  b <- freshE
  liftEffect $ Console.log (show b)
  pure $ { a, b }

freshMTL :: S.State Int Int
freshMTL = S.state \s -> Tuple s (s + 1)

exMTL :: S.State Int { a :: Int, b :: Int }
exMTL = do
  a <- freshMTL
  b <- freshMTL
  pure $ { a, b }

freshE :: forall m. S.MonadState Int m => m Int
freshE = S.state \s -> Tuple s (s + 1)

exE' :: S.StateT Int Effect { a :: Int, b :: Int }
exE' = do
  a <- freshE
  log (show a)
  b <- freshE
  -- log (show b)
  pure $ { a, b }

-- @inline Control.Monad.State.Trans.bindStateT(..).bind arity=2
-- @inline Control.Monad.State.Trans.applyStateT(..).apply arity=2
-- @inline Control.Monad.State.Trans.applicativeStateT(..).pure arity=1
-- @inline Control.Monad.State.Trans.functorStateT(..).map arity=2
-- @inline Control.Monad.State.Trans.monadStateStateT(..).state arity=1
module Snapshot.State where

import Prelude

import Control.Monad.State as S
import Data.Tuple (Tuple(..))
import Effect (Effect)

newtype State s a = State (s -> Tuple a s)
instance functorState :: Functor (State s) where
  map f (State state) = State \s ->
    case state s of
      Tuple a s' -> Tuple (f a) s'
instance applyState :: Apply (State s) where
  apply = ap
instance applicativeState :: Applicative (State s) where
  pure a = State \s -> Tuple a s
instance bindState :: Bind (State s) where
  bind (State state) f = State \s ->
    case state s of
      Tuple a s' ->
        case f a of
          State state' ->
            state' s'
instance monadState :: Monad (State s)

type S = State Int

fresh :: State Int Int
fresh = State \s -> Tuple s (s + 1)

ex :: State Int { a :: Int, b :: Int }
ex = do
  a <- fresh
  b <- fresh
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
  -- log (show a)
  b <- freshE
  -- log (show b)
  pure $ { a, b }

exE :: S.StateT Int Effect { a :: Int, b :: Int }
exE = do
  a <- freshE
  -- liftEffect $ Console.log (show a)
  b <- freshE
  -- S.StateT \s -> Tuple <$> Console.log (show b) <@> s
  pure $ { a, b }

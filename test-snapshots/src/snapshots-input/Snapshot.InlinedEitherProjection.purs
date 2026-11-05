module Snapshot.InlinedEitherProjection where

import Prelude

import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Assert (assertEqual)
import Unsafe.Coerce (unsafeCoerce)

-- https://github.com/id3as/purescript-backend-erl/issues/1
--
-- After inlining, the `case` on the `Either` that `extended` produces is a
-- constructor test distributed over the `Either`'s own `case`, and each use
-- of a field is another such `case`, with an arm per constructor. Under the
-- `Left` arm the `Right` projection is dead and ill-typed; once the string
-- append was distributed into it, it put a map in a `/binary` segment, which
-- erlc rejects. The dead arms must go, and a non-`expr_max` segment value
-- must be parenthesised regardless.

foreign import lookup :: Int -> Either String Int
foreign import check :: Int -> Maybe Int
foreign import crash :: forall a. String -> a

describe :: Int -> Effect Int
describe n = do
  let
    sets m = Just do
      x <- lookup m
      y <- note "clash" (check x)
      pure { track: x, renumbering: y }
    extended m = do
      k <- if m > 0 then Just m else Nothing
      joined <- sets k
      pure $ joined <#> \{ track, renumbering } -> { context: track + 1, renumbering }
  case extended n of
    Just (Right { context, renumbering }) -> pure (context + renumbering)
    Just (Left reason) -> crash ("cannot: " <> reason <> ".")
    Nothing -> pure 0

-- Independently of the dead arms: a segment value that is not an `expr_max`
-- (a map or record expression) has to be parenthesised, `(R#{..})/binary`.
segment :: { x :: Int } -> String
segment r = "x" <> unsafeCoerce (r { x = 1 })

main :: Effect Unit
main = do
  three <- describe 1
  assertEqual { expected: 3, actual: three }
  zero <- describe 0
  assertEqual { expected: 0, actual: zero }

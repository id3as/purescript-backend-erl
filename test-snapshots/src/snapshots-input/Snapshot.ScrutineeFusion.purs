-- | Scrutinee fusion: `case` over `Erl.Data.List.uncons` / `Erl.Data.Map.lookup`
-- | results should match the underlying value directly ([] / [H|T], maps:find)
-- | instead of allocating the intermediate Maybe/record. Cases where the Maybe
-- | value itself escapes must be left alone.
module Snapshot.ScrutineeFusion where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Assert (assertEqual)
import Data.Tuple (Tuple(..))
import Erl.Atom (Atom, atom)
import Erl.Data.List (List)

import Erl.Data.List as L
import Erl.Data.Map (Map)
import Erl.Data.Map as Map

-- Should fuse to `case L of [] -> 0; [H | T] -> ...`.
sumList :: List Int -> Int
sumList l = case L.uncons l of
  Nothing -> 0
  Just { head, tail } -> head + sumList tail

-- Refutable sub-pattern on the head: fusion must keep the fallthrough.
countJusts :: List (Maybe Int) -> Int
countJusts l = case L.uncons l of
  Nothing -> 0
  Just { head: Just _, tail } -> 1 + countJusts tail
  Just { head: Nothing, tail } -> countJusts tail

-- Only the tail is demanded.
len :: forall a. List a -> Int
len l = case L.uncons l of
  Nothing -> 0
  Just { tail } -> 1 + len tail

-- The Maybe value escapes: must NOT fuse.
escapes :: List Int -> Maybe { head :: Int, tail :: List Int }
escapes l = case L.uncons l of
  Nothing -> Nothing
  r -> r

-- Should fuse to `case maps:find(K, M) of error -> 0; {ok, V} -> V`.
getOr0 :: Map Atom Int -> Atom -> Int
getOr0 m k = case Map.lookup k m of
  Nothing -> 0
  Just v -> v

-- Nested value pattern through lookup (refutable payload; clause order kept).
lookupNested :: Map Atom (Tuple Int Int) -> Atom -> Int
lookupNested m k = case Map.lookup k m of
  Just (Tuple a 0) -> a
  Just (Tuple _ b) -> b
  Nothing -> -1

-- The Maybe value escapes whole: must NOT fuse (lookup analogue of `escapes`).
memoish :: Map Atom Int -> Atom -> Maybe Int
memoish m k = case Map.lookup k m of
  Nothing -> Nothing
  found -> found

main :: Effect Unit
main = do
  assertEqual { expected: Just 9, actual: memoish (Map.singleton (atom "m") 9) (atom "m") }
  assertEqual { expected: Nothing, actual: memoish (Map.singleton (atom "m") 9) (atom "q") }
  assertEqual { expected: 6, actual: result.sumList }
  assertEqual { expected: 2, actual: result.countJusts }
  assertEqual { expected: 2, actual: result.len }
  assertEqual { expected: Just { head: 7, tail: L.nil }, actual: result.escapes }
  assertEqual { expected: 0, actual: result.getOr0 }
  assertEqual { expected: 5, actual: result.lookupNested }
  assertEqual { expected: 4, actual: lookupNested (Map.singleton (atom "b") (Tuple 4 0)) (atom "b") }
  assertEqual { expected: -1, actual: lookupNested (Map.singleton (atom "b") (Tuple 4 0)) (atom "c") }

result :: { sumList :: Int, countJusts :: Int, len :: Int, escapes :: Maybe _, getOr0 :: Int, lookupNested :: Int }
result =
  { sumList: sumList (L.fromFoldable [ 1, 2, 3 ])
  , countJusts: countJusts (L.fromFoldable [ Just 1, Nothing, Just 3 ])
  , len: len (L.fromFoldable [ unit, unit ])
  , escapes: escapes (L.fromFoldable [ 7 ])
  , getOr0: getOr0 Map.empty (atom "a")
  , lookupNested: lookupNested (Map.singleton (atom "a") (Tuple 4 5)) (atom "a")
  }

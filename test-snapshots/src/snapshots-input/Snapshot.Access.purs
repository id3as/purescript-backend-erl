-- @inline Snapshot.Access.foo never
-- @inline Snapshot.Access.bar never
module Snapshot.Access where

import Prelude

import Data.Maybe (Maybe(..))

foo :: Int -> { x :: Int, y :: Int }
foo i = { x: i + 9, y: i - 5 }

bar :: { x :: Int, y :: Int } -> Int
bar { x, y } = (x - y) * y

f :: Int -> Int
f = \i ->
  case foo i of
    { x, y } -> x * y

g :: Int -> Int
g = \i ->
  case foo i of
    r@{ x, y } -> x * y + bar r

h :: Maybe Int -> Int
h = \mi ->
  case foo <$> mi of
    Nothing -> 0
    Just r@{ x, y } -> x * y + bar r


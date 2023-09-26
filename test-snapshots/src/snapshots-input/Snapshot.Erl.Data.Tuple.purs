-- @inline Snapshot.Erl.Data.Tuple.uncurriedMore never
-- @expected {14,<<"hitherehithere">>}
module Snapshot.Erl.Data.Tuple where

import Prelude

import Erl.Data.Tuple as T

t2 :: T.Tuple2 Int String
t2 = T.tuple2 4 "hi"

t5 :: T.Tuple5 Int Int String String Char
t5 = T.tuple5 3 4 "hi" "there" 'V'

tf2 :: T.Tuple2 (Int -> Int) String
tf2 = T.tuple2 (\i -> i - 5) "7"

fstt2 :: Int
fstt2 = T.fst t2
sndt2 :: String
sndt2 = T.snd t2

fsttf2 :: Int
fsttf2 = T.fst tf2 12

uncurried :: T.Tuple5 Int Int String String Char -> T.Tuple2 Int String
uncurried = T.uncurry5 \a b c d _e ->
  T.tuple2 (a + b) (c <> d)

uncurriedMore :: T.Tuple5 Int Int String String Char -> (Char -> String) -> T.Tuple2 Int String -> T.Tuple2 Int String
uncurriedMore =
  -- `f` is not part of uncurry5
  T.uncurry5 \a b c d e f ->
    T.uncurry2 \g h ->
      T.tuple2 (a + b + g) (c <> d <> f e <> h)

r5 :: T.Tuple2 Int String
r5 = t5 # T.uncurry5 \a b c d _e ->
  T.tuple2 (a + b) (c <> d)

result :: T.Tuple2 Int String
result = uncurriedMore t5 mempty r5

module Snapshot.PrimOps where

import Prelude

import Data.Int.Bits (shl, shr, xor, zshr, (.&.), (.|.))

booleanOps :: Boolean -> Boolean -> Array Boolean
booleanOps x y =
  [ x && y
  , x || y
  ]

intOps :: Int -> Int -> Array Int
intOps x y =
  [ x .&. y
  , x .|. y
  , x `shr` y
  , x `shl` y
  , x `zshr` y
  , x `xor` y
  , x + y
  , x / y
  , x * y
  , x - y
  ]

numberOps :: Number -> Number -> Array Number
numberOps x y =
  [ x + y
  , x / y
  , x * y
  , x - y
  ]

stringOps :: String -> String -> Array String
stringOps x y =
  [ x <> y
  ]

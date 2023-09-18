-- @inline Snapshot.ArrayMatch.bug28 never
-- @inline Snapshot.ArrayMatch.bug28_2 never
-- @inline Snapshot.ArrayMatch.nestedArray never
-- @inline Snapshot.ArrayMatch.nestedArrayViaRecord never
-- @inline Snapshot.ArrayMatch.nestedArrayRefutable never
-- @inline Snapshot.ArrayMatch.onlyArray never
-- @inline Snapshot.ArrayMatch.maybeArray never
-- @inline Snapshot.ArrayMatch.namedArray never
-- @expected array:from_list([3, 3, 4, 0, 0, 3, 3])
module Snapshot.ArrayMatch where

import Prelude

import Data.Maybe (Maybe(..))

result :: Array Int
result = do
  [ bug28 { q: [ 1, 2 ] }
  , bug28_2 { q: [ 1, 2 ] }
  , nestedArray [ [ 1, 2 ], [ 3 ]]
  , nestedArrayViaRecord { q: [ { r: [ 1, 2 ] }, { r: [ 3 ] } ] }
  , onlyArray [ 1 ]
  , maybeArray $ Just [ 1, 2 ]
  , namedArray [ 1, 2 ]
  ]

bug28 a =
  case a of
    { q: [a,b] } -> a + b
    _ -> 0

bug28_2 a = case { q: [ 1, 2 ] } of
  { q: s } -> case s of
    [ x, y ] -> x + y
    _ -> 0
  _ -> 0

nestedArray =
  case _ of
    [ [a,b], _ ] -> a + b + 1
    -- [ [c], _ ] -> c
    _ -> 0

nestedArrayViaRecord =
  case _ of
    { q: [ { r: [ a, b ]}] } -> a + b
    { q: [ { r: [ a ]}, { r: [ b ] } ] } -> a + b
    _ -> 0

nestedArrayRefutable arg1 arg2 =
  case arg1, arg2 of
    [ [a,b,3], _ ], 2 -> a + b + 1
    -- [ [c], _ ] -> c
    _, _ -> 0

nestedArrayRefutable2 =
  case _, _ of
    [ [a,b,3], _ ], 2 -> a + b + 1
    -- [ [c], _ ] -> c
    _, _ -> 0


onlyArray a = case a of
  [ a, b ] -> a + b
  _ -> 0

maybeArray a = case a of
  Just [ a, b ] -> a + b
  _ -> 0


namedArray = case _ of
  foo@[a,b] -> a+b
  _ -> 0






-- --
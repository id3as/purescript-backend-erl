module Snapshot.Generic where

import Data.Generic.Rep (class Generic)

data ADT
  = A
  | D Int
  | T String ADT

derive instance genericADT :: Generic ADT _

data AnEnum
  = E1
  | E2
  | E3
  | E4
  | E5
  | E6
  | E7
  | E8

derive instance genericAnEnum :: Generic AnEnum _

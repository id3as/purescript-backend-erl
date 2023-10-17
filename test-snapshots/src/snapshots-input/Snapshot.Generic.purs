module Snapshot.Generic where

import Prelude
import Data.Generic.Rep (class Generic)

data ADT
  = A
  | D Int
  | T String ADT

derive instance genericADT :: Generic ADT _

module Snapshot.CSE where

import Prelude

data D = D Int Int

ord (D a b) (D c d) = compare a c <> compare b d

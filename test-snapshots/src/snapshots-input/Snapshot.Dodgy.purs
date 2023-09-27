-- @inline Snapshot.Dodgy.noInline never
-- @expected false
module Snapshot.Dodgy where

import Unsafe.Coerce (unsafeCoerce)

data PrivateProcessTTimeoutMsg
  = PrivateProcessTTimeoutMsg__
  | ThereToGetRidOfUnreachableWarning

noInline :: forall a. a -> a
noInline a = a

isSecretMsg :: PrivateProcessTTimeoutMsg -> Boolean
isSecretMsg PrivateProcessTTimeoutMsg__ = true
isSecretMsg _ = false

result :: Boolean
result = isSecretMsg (noInline (unsafeCoerce 1))

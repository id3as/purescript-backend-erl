-- @inline Snapshot.Prime.foo never
-- @inline Snapshot.Prime.foo' never
-- @inline Snapshot.Prime.foo'' never
-- @inline Snapshot.Prime.foo'oo never
-- @inline Snapshot.Prime.ignore never
-- @inline Snapshot.Prime.classMember' never
-- @inline Snapshot.Prime.normal never
-- @expected #{test1=>true, test2=>true, test3=>true, useInstance=><<"F1F2">>}
module Snapshot.Prime where

import Data.Eq ((==))
import Data.Semigroup ((<>))

foo :: String
foo = "foo"

-- declarations ending in prime shouldn't cause issues
foo' :: String
foo' = "foo'"

-- declarations ending in prime shouldn't cause issues
foo'' :: String
foo'' = "foo''"

foo'oo :: String
foo'oo = "foo'oo"

data DataType' = DCtor

data DataCtor = Ctor' String Int

newtype NewtypeType' = NCtor Int

newtype NewtypeCtor = NewtypeCtor' Int

class ClassName' a where
  ignore :: a -> String

class ClassMember a where
  classMember' :: a -> String

data Foo
  = F1
  | F2

class Normal a where
  normal :: a -> String

instance instanceName' :: Normal Foo where
  normal = case _ of
    F1 -> "F1"
    F2 -> "F2"

useFooPrime1 :: String
useFooPrime1 = foo'

useFooPrime2 :: String
useFooPrime2 = foo''

useFooPrime3 :: String
useFooPrime3 = foo'oo

useDataType :: String -> DataType'
useDataType _ = DCtor

useDataCtor :: String -> DataCtor
useDataCtor s = Ctor' s 4

useNewtypeType :: Int -> NewtypeType'
useNewtypeType i = NCtor i

useNewtypeCtor :: Int -> NewtypeCtor
useNewtypeCtor i = NewtypeCtor' i

useClass :: forall a. ClassName' a => a -> String
useClass x = ignore x

useMember :: forall a. ClassMember a => a -> String
useMember x = classMember' x

useNormal :: forall a b. Normal a => Normal b => a -> b -> String
useNormal a b = normal a <> normal b

useInstance :: String
useInstance = useNormal F1 F2

result :: { test1 :: Boolean, test2 :: Boolean, test3 :: Boolean, useInstance :: String }
result =
  { test1: useFooPrime1 == foo'
  , test2: useFooPrime2 == foo''
  , test3: useFooPrime3 == foo'oo
  , useInstance: useNormal F1 F2
  }

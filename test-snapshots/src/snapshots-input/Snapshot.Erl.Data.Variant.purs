-- -- @inline Erl.Data.Variant.match arity=1
-- @inline Erl.Data.Variant.match never

-- @inline Erl.Data.Variant.on arity=2
-- @inline Erl.Data.Variant.prj arity=3
-- @inline Erl.Data.Variant.eqVariant arity=2
-- @inline Erl.Data.Variant.eqVariantNil always
-- @inline Erl.Data.Variant.eqVariantCons arity=2
-- @inline Erl.Data.Variant.Internal.variantTagsNil always
-- @inline Erl.Data.Variant.Internal.variantTagsCons arity=2
-- @inline Erl.Data.Variant.Internal.contractWithInstance(..).contractWith always

-- @inline Snapshot.Erl.Data.Variant.noInline never

-- @expected {{6, 7, 8}, {<<"1">>, <<"2">>}, {<<"5">>, <<"\"hi\"">>, <<"[unit,unit,unit]">>}}
module Snapshot.Erl.Data.Variant where

import Erl.Data.Variant
import Prelude
import Erl.Data.Tuple

import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

type V = Variant ( a :: Int, b :: String, c :: Array Unit )

test1 :: Partial => V -> Int
test1 = match
  { a: \5 -> 6
  , b: \"5" -> 7
  , c: \[_,_,_,_,_] -> 8
  }

test2 :: Partial => V -> V -> Tuple2 String String
test2 = match
  { a: \a1 -> match
    { a: \a2 -> tuple2 (show a1) (show a2)
    , b: \b2 -> tuple2 (show a1) b2
    , c: \c2 -> tuple2 (show a1) (show c2)
    }
  , b: flip $ match
    { a: \a2 b1 -> tuple2 b1 (show a2)
    , b: \b2 b1 -> tuple2 b1 b2
    , c: \c2 b1 -> tuple2 b1 (show c2)
    }
  , c: \c1 v2 -> tuple2 (show c1) (show v2)
  }

test3 :: V -> String
test3 = case_
  # on (Proxy :: Proxy "a") show
  # on (Proxy :: Proxy "b") show
  # on (Proxy :: Proxy "c") show

noInline :: forall a. a -> a
noInline a = a

result :: Tuple3 (Tuple3 Int Int Int) _ (Tuple3 String String String)
result = unsafePartial do
  tuple3
    do
      tuple3
        do noInline test1 (inj (Proxy :: Proxy "a") 5)
        do noInline test1 (inj (Proxy :: Proxy "b") "5")
        do noInline test1 (inj (Proxy :: Proxy "c") [unit,unit,unit,unit,unit])
    do
      do noInline test2 (inj (Proxy :: Proxy "a") 1) (inj (Proxy :: Proxy "a") 2)
    do
      tuple3
        do noInline test3 (inj (Proxy :: Proxy "a") 5)
        do noInline test3 (inj (Proxy :: Proxy "b") "hi")
        do noInline test3 (inj (Proxy :: Proxy "c") [unit, unit, unit])

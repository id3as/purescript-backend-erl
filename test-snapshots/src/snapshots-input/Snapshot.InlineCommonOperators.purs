module Snapshot.InlineCommonOperators where

import Prelude
import Effect
import Effect.Class
import Erl.Data.List
import Erl.Data.List as List
import Erl.Atom
import Unsafe.Coerce
import Safe.Coerce
import Data.Int as Int
import Data.Newtype

inlineBinary :: Int -> Number -> Boolean -> String -> Char -> _
inlineBinary i n b s c = do
  let divInt = i / i
      divNum = n / n
      andBool = b && b
      orBool = b || b
      appendList (l1 :: List Int) l2 = l1 <> l2

      addInt = i + i
      mulInt = i * i
      subInt = i - i
      addNum = n + n
      mulNum = n * n
      subNum = n - n

      eqNum = n == n
      notEqNum = n /= n
      eqInt = i == i
      notInt = i /= i
      eqString = s == s
      notEqString = s /= s
      eqChar = c == c
      notEqChar = c /= c
      eqBoolean = b == b
      notEqBoolean = b == b

      ltInt = i < i
      lteInt = i <= i
      gtInt = i > i
      gteInt = i >= i
      minInt = min i i
      maxInt = max i i

  { divInt
  , divNum
  , andBool
  , orBool
  , appendList
  , addInt
  , mulInt
  , subInt
  , addNum
  , mulNum
  , subNum
  , eqNum
  , notEqNum
  , eqInt
  , notInt
  , eqString
  , notEqString
  , eqChar
  , notEqChar
  , eqBoolean
  , notEqBoolean
  , ltInt
  , lteInt
  , gtInt
  , gteInt
  , minInt
  , maxInt
  }

inlineUnary :: Int -> Number -> Boolean -> _
inlineUnary i n b = do
  let negNum = - n
      negInt = - i
      notBoolean = not b

  { negNum
  , negInt
  , notBoolean
  }

inlineListCons x (xs :: List Int) = x : xs
inlineListSingleton = List.singleton 1
inlineAtom = atom "an_atom"
inlineVoid = void (pure 49 :: Effect _)
stringAppend world = "Hello " <> world

inlineUnsafeCoerce :: Int -> String
inlineUnsafeCoerce = unsafeCoerce 42

inlineIntToNumber = Int.toNumber 42

newtype N a = N a
derive instance Newtype (N a) _

inlineCoerce :: Int
inlineCoerce = coerce (N 42)

inlineUnwrap = unwrap (N 1) :: Int
inlineWrap = wrap 1 :: N Int

inlineOver :: N Int -> N Int
inlineOver = over N (_+1)

inlineOver2 :: N Int -> N Int -> N Int
inlineOver2 = over2 N (+)

-- discard Unit - see magicdo tests
-- onNFn - see fnN tests

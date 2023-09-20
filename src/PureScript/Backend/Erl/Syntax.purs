module PureScript.Backend.Erl.Syntax where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), uncurry)

type ErlModule =
  { moduleName :: String
  , definitions :: Array ErlDefinition
  , exports :: Array ErlExport
  }

data ErlExport = Export String Int

data ErlDefinition
  -- |
  -- Top-level function definition
  --
  = FunctionDefinition {- (Maybe EType) (Maybe SourceSpan) -}  String (Array String) ErlExpr

data ErlExpr
  = Literal ErlLiteral
  | Var String
  | List (Array ErlExpr)
  | ListCons (Array ErlExpr) ErlExpr
  -- | An erlang tuple { E1, E2, ... }
  | Tupled (Array ErlExpr)
  -- | A map expression #{ X => E }
  | Map (Array (Tuple String ErlExpr))
  -- | A map update expression M#{ X => E }
  | MapUpdate ErlExpr (Array (Tuple String ErlExpr))

  -- | E1 = E2
  | Match ErlExpr ErlExpr
  | Block (Array ErlExpr)
  -- |
  -- A fun definition
  --
  | Fun (Maybe String) (Array (Tuple FunHead ErlExpr))
  -- |
  -- A (possibly qualified) function call
  | FunCall (Maybe ErlExpr) ErlExpr (Array ErlExpr)
  | If (NonEmptyArray IfClause)
  | Case ErlExpr (NonEmptyArray CaseClause)
  | BinOp BinaryOperator ErlExpr ErlExpr
  | UnaryOp UnaryOperator ErlExpr

  | Unimplemented String

data FunHead = FunHead (Array ErlExpr) (Maybe Guard)
data IfClause = IfClause ErlExpr ErlExpr

data CaseClause = CaseClause ErlExpr (Maybe ErlExpr) ErlExpr

newtype Guard = Guard ErlExpr

data ErlLiteral
  = Integer Int
  | Float Number
  | String String
  | Char Char
  | Atom String

-- |
-- Built-in binary operators
--
data BinaryOperator
  -- |
  -- Numeric addition
  --
  = Add
  -- |
  -- Numeric subtraction
  --
  | Subtract
  -- |
  -- Numeric multiplication
  --
  | Multiply
  -- |
  -- Numeric division (float)
  --
  | FDivide
  -- |
  -- Numeric division (integer)
  --
  | IDivide
  -- |
  -- Remainder
  --
  | Remainder
  -- |
  -- Generic equality test
  --
  | EqualTo
  -- |
  -- Generic inequality test
  --
  | NotEqualTo
  -- |
  -- Generic identical test
  --
  | IdenticalTo
  -- |
  -- Generic non-identical test
  --
  | NotIdenticalTo
  -- |
  -- Numeric less-than
  --
  | LessThan
  -- |
  -- Numeric less-than-or-equal
  --
  | LessThanOrEqualTo
  -- |
  -- Numeric greater-than
  --
  | GreaterThan
  -- |
  -- Numeric greater-than-or-equal
  --
  | GreaterThanOrEqualTo

  -- |
  -- Boolean and
  --
  | And
  -- |
  -- Boolean or
  --
  | Or
  -- |
  -- Boolean short-circuit and
  --
  | AndAlso
  -- |
  -- Boolean short-circuit or
  --
  | OrElse
  -- |
  -- Boolean xor
  --
  | XOr
  -- |
  -- Bitwise and
  --
  | BitwiseAnd
  -- |
  -- Bitwise or
  --
  | BitwiseOr
  -- |
  -- Bitwise xor
  --
  | BitwiseXor
  -- |
  -- Bitwise left shift
  --
  | ShiftLeft
  -- |
  -- Bitwise right shift
  --
  | ShiftRight

  -- |
  -- List concatenation (++)
  --
  | ListConcat
  -- |
  -- List subtraction (--)
  --
  | ListSubtract

-- |
-- Built-in unary operators
--
data UnaryOperator
  -- |
  -- Numeric negation
  --
  = Negate
  -- |
  -- Boolean negation
  --
  | Not
  -- |
  -- Bitwise negation
  --
  | BitwiseNot
  -- |
  -- Numeric unary \'plus\'
  --
  | Positive

curriedApp :: forall f. Foldable f => ErlExpr -> f ErlExpr -> ErlExpr
curriedApp f es = foldl (\e e' -> FunCall Nothing e [ e' ]) f es

curriedFun :: forall f. Foldable f => f ErlExpr -> ErlExpr -> ErlExpr
curriedFun args e =
  foldr go e args
  where
  go :: ErlExpr -> ErlExpr -> ErlExpr
  go a eAcc = Fun Nothing
    [ Tuple
        (FunHead [ a ] Nothing)
        eAcc
    ]

simpleFun :: Array ErlExpr -> ErlExpr -> ErlExpr
simpleFun args e =
  Fun Nothing [ Tuple (FunHead args Nothing) e ]

assignments :: Array (Tuple String ErlExpr) -> ErlExpr -> ErlExpr
assignments [] res = res
assignments bindings res =
  Block $ (uncurry Match <<< lmap Var <$> bindings) `Array.snoc` res

atomLiteral :: String -> ErlExpr
atomLiteral = Literal <<< Atom

intLiteral :: Int -> ErlExpr
intLiteral = Literal <<< Integer

numberLiteral :: Int -> ErlExpr
numberLiteral = Literal <<< Integer

stringLiteral :: String -> ErlExpr
stringLiteral = Literal <<< String

thunk :: ErlExpr -> ErlExpr
thunk = simpleFun []

unthunk :: ErlExpr -> ErlExpr
unthunk e = FunCall Nothing e []

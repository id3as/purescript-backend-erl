module PureScript.Backend.Erl.Syntax where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (lmap)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd, uncurry)
import PureScript.Backend.Erl.Constants as C

type ErlModule =
  { moduleName :: String
  , definitions :: Array ErlDefinition
  , exports :: Array ErlExport
  , comments :: Array String
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
  | Macro String (Maybe (NonEmptyArray ErlExpr))
  | If (NonEmptyArray IfClause)
  | Case ErlExpr (NonEmptyArray CaseClause)
  | BinOp BinaryOperator ErlExpr ErlExpr
  | UnaryOp UnaryOperator ErlExpr
  | BinaryAppend ErlExpr ErlExpr

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

visit :: forall m. Monoid m => (ErlExpr -> m) -> ErlExpr -> m
visit f = go
  where
  goes :: Array ErlExpr -> m
  goes es = foldMap go es
  goFunHead :: FunHead -> m
  goFunHead (FunHead es mg) = goes es <> foldMap (\(Guard g) -> go g) mg
  goIfClause (IfClause e1 e2) = go e1 <> go e2
  goCaseClause (CaseClause e1 me2 e3) = go e1 <> foldMap go me2 <> go e3
  go e0 = f e0 <> case e0 of
    Literal _ -> mempty
    Var _ -> mempty
    List es -> goes es
    ListCons es e -> goes es <> go e
    Tupled es -> goes es
    Map kvs -> foldMap (go <<< snd) kvs
    MapUpdate e kvs -> go e <> foldMap (go <<< snd) kvs
    Match e1 e2 -> go e1 <> go e2
    Block es -> goes es
    Fun _ heads -> foldMap (bifoldMap goFunHead go) heads
    FunCall me e es -> foldMap go me <> go e <> goes es
    Macro _ mes -> foldMap (foldMap go) mes
    If cs -> foldMap goIfClause cs
    Case e cs -> go e <> foldMap goCaseClause cs
    BinOp _ e1 e2 -> go e1 <> go e2
    UnaryOp _ e1 -> go e1
    BinaryAppend e1 e2 -> go e1 <> go e2

macros :: ErlExpr -> Set.Set String
macros = visit case _ of
  Macro name _ -> Set.singleton name
  _ -> mempty

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

binops :: ErlExpr -> BinaryOperator -> Array ErlExpr -> ErlExpr
binops z op = NEA.fromArray >>> maybe z (binops' op)

binops' :: BinaryOperator -> NonEmptyArray ErlExpr -> ErlExpr
binops' op = NEA.foldr1 (BinOp op)


predefMacros :: Array (Tuple (Tuple String (Maybe (NonEmptyArray String))) ErlExpr)
predefMacros =
  [ Tuple (Tuple "IS_TAG" (NEA.fromArray [ "Tag", "V" ])) $
      binops (Literal (Atom "true")) AndAlso
      [ FunCall (Just $ atomLiteral C.erlang) (atomLiteral "is_tuple") [ Var "V" ]
      , BinOp LessThanOrEqualTo (Literal (Integer 1)) $
          FunCall (Just $ atomLiteral C.erlang) (atomLiteral "tuple_size") [ Var "V" ]
      , BinOp IdenticalTo (Var "Tag") $
          FunCall (Just $ atomLiteral C.erlang) (atomLiteral C.element)
            [ numberLiteral 1, Var "V" ]
      ]
  ]

mIS_TAG :: ErlExpr -> ErlExpr -> ErlExpr
mIS_TAG tag v = Macro "IS_TAG" $ NEA.fromArray [ tag, v ]

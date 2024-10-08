module PureScript.Backend.Erl.Syntax where

import Prelude

import Data.Array (all)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (lmap)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, sum)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)
import PureScript.Backend.Erl.Constants as C
import Safe.Coerce (coerce)

type ErlModule =
  { moduleName :: String
  , definitions :: Array ErlDefinition
  , rawDefinitions :: Array ErlDefinition
  , exports :: Array ErlExport
  , comments :: Array String
  }

data ErlExport = Export String Int

data ErlDefinition
  -- |
  -- Top-level function definition
  --
  = FunctionDefinition {- (Maybe EType) (Maybe SourceSpan) -} String (Array ErlPattern) ErlExpr

data ErlExpr
  = Literal ErlLiteral
  | Var String Accessors
  | List (Array ErlExpr)
  | ListCons (Array ErlExpr) ErlExpr
  -- | An erlang tuple { E1, E2, ... }
  | Tupled (Array ErlExpr)
  -- | A map expression #{ X => E }
  | Map (Array (Tuple ErlExpr ErlExpr))
  -- | A map update expression M#{ X => E }
  | MapUpdate ErlExpr (Array (Tuple ErlExpr ErlExpr))
  -- | A map expression #{ X => E }
  | Record (Array (Tuple String ErlExpr))
  -- | A map update expression M#{ X => E }
  | RecordUpdate ErlExpr (Array (Tuple String ErlExpr))

  | Assignments (Array (Tuple ErlPattern ErlExpr)) ErlExpr
  -- | A fun definition
  | Fun (Maybe String) (Array (Tuple FunHead ErlExpr))
  -- | A (possibly qualified) function call
  | FunCall (Maybe ErlExpr) ErlExpr (Array ErlExpr)
  -- | The name of a function, possibly qualified
  -- | fun mod:fn/2
  | FunName (Maybe ErlExpr) ErlExpr Int
  | Macro String (Maybe (NonEmptyArray ErlExpr))
  | If (NonEmptyArray IfClause)
  | Case ErlExpr (NonEmptyArray CaseClause)
  | BinOp BinaryOperator ErlExpr ErlExpr
  | UnaryOp UnaryOperator ErlExpr
  | BinaryAppend ErlExpr ErlExpr

-- Only used in Test.Eval
derive instance eqErlExpr :: Eq ErlExpr

type Accessors = Array Accessor
data Accessor
  = AcsElement Int
  | AcsKey String
  | AcsItem Int
  | AcsDrop Int
  -- | AcsTag String
derive instance eqAccessor :: Eq Accessor
derive instance ordAccessor :: Ord Accessor

data ErlPattern
  = Discard
  | BindVar String
  | MatchLiteral ErlLiteral
  -- | Match operator, X = P1
  | MatchBoth String ErlPattern
  -- | A map pattern #{ X := E }
  | MatchMap (Array (Tuple String ErlPattern))
  | MatchTuple (Array ErlPattern)
  | MatchList (Array ErlPattern) (Maybe ErlPattern)
derive instance Eq ErlPattern

data FunHead = FunHead (Array ErlPattern) (Maybe Guard)
data IfClause = IfClause Guard ErlExpr
data CaseClause = CaseClause ErlPattern (Maybe Guard) ErlExpr
derive instance eqFunHead :: Eq FunHead
derive instance eqIfClause :: Eq IfClause
derive instance eqCaseClause :: Eq CaseClause

newtype Guard = Guard ErlExpr
derive instance Newtype Guard _
derive newtype instance eqGuard :: Eq Guard

data ErlLiteral
  = Integer Int
  | Float Number
  | String String
  | Char Char
  | Atom String
derive instance Eq ErlLiteral

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

derive instance eqBinaryOperator :: Eq BinaryOperator

shortCircuits :: BinaryOperator -> Boolean
shortCircuits AndAlso = true
shortCircuits OrElse = true
shortCircuits _ = false

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

derive instance eqUnaryOperator :: Eq UnaryOperator

--------------------------------------------------------------------------------
-- AST traversals                                                             --
--------------------------------------------------------------------------------

-- | Simple traversal: append the values of all nodes together (not just leaf
-- | nodes)
visit :: forall m. Monoid m => (ErlExpr -> m) -> ErlExpr -> m
visit f = visit' \expr -> Append (f expr)

data Visit m
  = ShortCircuit m
  | Append m
  | Continue (m -> m)

-- | Traverse with the ability to short circuit and alter results of child
-- | traversals
visit' :: forall m. Monoid m => (ErlExpr -> Visit m) -> ErlExpr -> m
visit' f = go
  where
  goes :: Array ErlExpr -> m
  goes es = foldMap go es
  goFunHead :: FunHead -> m
  goFunHead (FunHead _ps mg) = foldMap (coerce go) mg
  goIfClause (IfClause e1 e2) = coerce go e1 <> go e2
  goCaseClause (CaseClause _p1 me2 e3) = foldMap (coerce go) me2 <> go e3
  go e0 = case f e0 of
    ShortCircuit m -> m
    Append m -> m <> children e0
    Continue mm -> mm (children e0)
  children = case _ of
      Literal _ -> mempty
      Var _ _ -> mempty
      List es -> goes es
      ListCons es e -> goes es <> go e
      Tupled es -> goes es
      Map kvs -> foldMap (go <<< snd) kvs
      MapUpdate e kvs -> go e <> foldMap (go <<< snd) kvs
      Record kvs -> foldMap (go <<< snd) kvs
      RecordUpdate e kvs -> go e <> foldMap (go <<< snd) kvs
      Assignments es e -> foldMap (go <<< snd) es <> go e
      Fun _ heads -> foldMap (bifoldMap goFunHead go) heads
      FunCall me e es -> foldMap go me <> go e <> goes es
      FunName me e _ -> foldMap go me <> go e
      Macro _ mes -> foldMap (foldMap go) mes
      If cs -> foldMap goIfClause cs
      Case e cs -> go e <> foldMap goCaseClause cs
      BinOp _ e1 e2 -> go e1 <> go e2
      UnaryOp _ e1 -> go e1
      BinaryAppend e1 e2 -> go e1 <> go e2

-- | AST size
termSize :: ErlExpr -> Int
termSize = unwrap <<< visit (const (Additive 1))

-- | The AST size of declarations in the module
moduleSize :: ErlModule -> Int
moduleSize { definitions } = sum $ definitions <#>
  \(FunctionDefinition _ _ expr) -> termSize expr

-- | Get the set of macros used in the expression
macros :: ErlExpr -> Set.Set String
macros = visit case _ of
  Macro name _ -> Set.singleton name
  _ -> mempty


data Complexity
  = Lit Int
  | Complex Int
instance semigroupComplexity :: Semigroup Complexity where
  append (Complex i) (Complex j) = Complex (i + j)
  append (Lit i) (Complex j) = Complex (i + j)
  append (Complex i) (Lit j) = Complex (i + j)
  append (Lit i) (Lit j) = Lit (i + j)
instance monoidComplexity :: Monoid Complexity where
  mempty = Lit 0

unComplexity :: Complexity -> Int
unComplexity (Lit _) = 1
unComplexity (Complex i) = i

-- | Estimate the runtime complexity of an expression. Used for determining when
-- | to memoize a function.
estimatedComplexity :: ErlExpr -> Int
estimatedComplexity = unComplexity <<< visit' case _ of
  -- Do not recurse into closures
  Fun _ _ -> ShortCircuit (Complex 1)
  -- Yeah, `fun f/1` is an allocation ...
  FunName _ _ _ -> ShortCircuit (Complex 1)
  -- A curried call costs one
  FunCall Nothing (FunCall _ _ _) _ -> Append (Complex 1)
  -- Base calls cost more since they might do more work
  -- (Note: atoms count during recursion, so unqualitifed >= 3,
  -- and qualified calls >= 4, plus arguments of course)
  FunCall _ _ _ -> Append (Complex 2)
  -- Literals are cheap
  Literal _ -> ShortCircuit (Lit 1)
  -- Literal constructors are cheap if their children are literals
  List _ -> groupOfLiterals
  Tupled _ -> groupOfLiterals
  Map _ -> groupOfLiterals
  Record _ -> groupOfLiterals
  -- Everything else costs 1 plus its children
  _ -> Append (Complex 1)
  where
  groupOfLiterals :: Visit Complexity
  groupOfLiterals = Continue case _ of
    Lit _ -> Lit 1
    Complex i -> Complex (i + 1)

-- | Check whether an expression is a guard expression, able to be used in if
-- | expressions and case guards
guardExpr :: ErlExpr -> Boolean
guardExpr = case _ of
  Literal _ -> true
  Var _ _ -> true
  List items -> all guardExpr items
  ListCons items tail -> all guardExpr items && guardExpr tail
  Tupled items -> all guardExpr items
  Map items -> all (guardExpr <<< snd) items
  MapUpdate e items -> guardExpr e && all (guardExpr <<< snd) items
  FunCall (Just (Literal (Atom mod))) (Literal (Atom fn)) args ->
    Array.elem (Tuple mod fn) guardFns && all guardExpr args
  BinOp _ e1 e2 -> guardExpr e1 && guardExpr e2
  UnaryOp _ e -> guardExpr e
  BinaryAppend e1 e2 -> guardExpr e1 && guardExpr e2
  Macro "IS_KNOWN_TAG" margs -> maybe false (all guardExpr <<< NEA.toArray) margs
  _ -> false

guardFns :: Array (Tuple String String)
guardFns = map (Tuple C.erlang)
  [ "abs"
  , "binary_part"
  , "bit_size"
  , "byte_size"
  , "ceil"
  , "element"
  , "float"
  , "floor"
  , "hd"
  , "is_atom"
  , "is_binary"
  , "is_bitstring"
  , "is_boolean"
  , "is_float"
  , "is_function"
  , "is_integer"
  , "is_list"
  , "is_map"
  , "is_map_key"
  , "is_number"
  , "is_pid"
  , "is_port"
  -- , "is_record" -- has some side-conditions
  , "is_reference"
  , "is_tuple"
  , "length"
  , "map_get"
  , "map_size"
  , "max"
  , "min"
  , "node"
  , "round"
  , "self"
  , "size"
  , "tl"
  , "trunc"
  , "tuple_size"
  ]

--------------------------------------------------------------------------------
-- AST construction helpers                                                   --
--------------------------------------------------------------------------------


curriedApp :: forall f. Foldable f => ErlExpr -> f ErlExpr -> ErlExpr
curriedApp f es = foldl (\e e' -> FunCall Nothing e [ e' ]) f es

curriedFun :: forall f. Foldable f => f ErlPattern -> ErlExpr -> ErlExpr
curriedFun args e =
  foldr go e args
  where
  go :: ErlPattern -> ErlExpr -> ErlExpr
  go a eAcc = Fun Nothing
    [ Tuple
        (FunHead [ a ] Nothing)
        eAcc
    ]

callGlobal :: String -> String -> Array ErlExpr -> ErlExpr
callGlobal mod fn = FunCall (Just (atomLiteral mod)) (atomLiteral fn)

simpleFun :: Array ErlPattern -> ErlExpr -> ErlExpr
simpleFun args e =
  Fun Nothing [ Tuple (FunHead args Nothing) e ]

assignments :: Array (Tuple String ErlExpr) -> ErlExpr -> ErlExpr
assignments [] res = res
assignments bindings res =
  Assignments (lmap bindOrDiscard <$> bindings) res

bindOrDiscard :: String -> ErlPattern
bindOrDiscard "_" = Discard
bindOrDiscard v = BindVar v

optimizePattern :: ErlPattern -> ErlPattern
optimizePattern Discard = Discard
optimizePattern (BindVar "_") = Discard
optimizePattern pat@(BindVar _) = pat
optimizePattern (MatchBoth "_" pat) = optimizePattern pat
optimizePattern (MatchBoth v pat) = case optimizePattern pat of
  Discard -> BindVar v
  pat' -> MatchBoth v pat'
optimizePattern (MatchMap items) =
  case Array.filter (notEq Discard <<< snd) (map optimizePattern <$> items) of
    [] -> Discard
    items' -> MatchMap items'
optimizePattern (MatchTuple items) = MatchTuple (optimizePattern <$> items)
optimizePattern pat@(MatchLiteral _) = pat
optimizePattern (MatchList [] (Just pat)) = optimizePattern pat
optimizePattern (MatchList items tail) =
  MatchList
    (optimizePattern <$> items)
    (optimizePattern <$> tail)

self :: Accessors
self = []

access :: Accessor -> ErlExpr -> ErlExpr
access = access' true

applyAccessors :: ErlExpr -> Accessors -> ErlExpr
applyAccessors = applyAccessors' false

applyAccessors' :: Boolean -> ErlExpr -> Accessors -> ErlExpr
applyAccessors' b = foldl (\e a -> access' b a e)

access' :: Boolean -> Accessor -> ErlExpr -> ErlExpr
access' true acs (Var v acrs) = Var v (acrs <> [acs])
access' _ (AcsElement n) e = callGlobal "erlang" "element" [intLiteral n, e]
access' _ (AcsKey k) e = callGlobal "erlang" "map_get" [atomLiteral k, e]
access' _ (AcsItem n) e = callGlobal "lists" "nth" [intLiteral n, e]
access' _ (AcsDrop n) e = callGlobal "lists" "nthtail" [intLiteral n, e]

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

unthunk' :: ErlExpr -> ErlExpr
unthunk' (Fun Nothing [ Tuple (FunHead [] Nothing) e ]) = e
unthunk' e = FunCall Nothing e []

binops :: ErlExpr -> BinaryOperator -> Array ErlExpr -> ErlExpr
binops z op = NEA.fromArray >>> maybe z (binops' op)

binops' :: BinaryOperator -> NonEmptyArray ErlExpr -> ErlExpr
binops' op = NEA.foldr1 (BinOp op)


predefMacros :: Array (Tuple (Tuple String (Maybe (NonEmptyArray String))) ErlExpr)
predefMacros =
  [ Tuple (Tuple "IS_KNOWN_TAG" (NEA.fromArray [ "Tag", "Arity", "V" ])) $
      binops (Literal (Atom "true")) AndAlso
      [ FunCall (Just $ atomLiteral C.erlang) (atomLiteral "is_tuple") [ Var "V" self ]
      , BinOp IdenticalTo (BinOp Add (Var "Arity" self) (intLiteral 1)) $
          FunCall (Just $ atomLiteral C.erlang) (atomLiteral "tuple_size") [ Var "V" self ]
      , BinOp IdenticalTo (Var "Tag" self) $
          FunCall (Just $ atomLiteral C.erlang) (atomLiteral C.element)
            [ numberLiteral 1, Var "V" self ]
      ]
  , Tuple (Tuple "MEMOIZE_AS" (NEA.fromArray [ "Key", "_Metadata", "Expr" ])) $
      Case (FunCall (Just $ atomLiteral "persistent_term") (atomLiteral "get") [ Var "Key" self, atomLiteral "undefined" ])
        let
          ifNotFound = CaseClause (MatchLiteral (Atom "undefined")) Nothing $
            Assignments
              [ Tuple (BindVar "MemoizeAsResult") (Var "Expr" self)
              , Tuple Discard (FunCall (Just $ atomLiteral "persistent_term") (atomLiteral "put") [ Var "Key" self, Var "MemoizeAsResult" self ])
              ]
              (Var "MemoizeAsResult" self)
          ifFound = CaseClause (BindVar "MemoizeAsResult") Nothing
            (Var "MemoizeAsResult" self)
        in NEA.cons' ifNotFound [ ifFound ]
  ]

mIS_KNOWN_TAG :: ErlExpr -> Int -> ErlExpr -> ErlExpr
mIS_KNOWN_TAG tag arity v = Macro "IS_KNOWN_TAG" $ NEA.fromArray [ tag, Literal (Integer arity), v ]

mMEMOIZE_AS :: Array String -> ErlExpr -> ErlExpr -> ErlExpr
mMEMOIZE_AS keys metadata expr = Macro "MEMOIZE_AS" $ NEA.fromArray [ key, metadata, expr ]
  where
  key = Tupled $ atomLiteral <$> (keys <> [ "(memoized)" ])

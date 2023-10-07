module PureScript.Backend.Erl.Calling where

import Prelude

import Control.Alt (class Alt, alt, (<|>))
import Control.Alternative (guard)
import Control.Apply (lift2)
import Control.Plus (empty)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (class Bifunctor)
import Data.Compactable (class Compactable, compact, separateDefault)
import Data.Either (Either(..), hush)
import Data.Filterable (class Filterable, filterDefault, partitionDefault, partitionMapDefault)
import Data.Foldable (class Foldable, foldl, sum)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Lazy (defer, force)
import Data.Lens (APrism', Prism', preview, prism', review, withPrism)
import Data.List (List(..))
import Data.List as List
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (class Newtype, un, unwrap, wrap)
import Data.Profunctor (class Profunctor)
import Data.Semigroup.Last (Last(..))
import Data.String as String
import Data.Traversable (class Traversable, for, mapAccumR, sequence, traverse)
import Data.Tuple (Tuple(..), fst, uncurry)
import Effect.Exception (catchException)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import PureScript.Backend.Erl.Convert.Common (erlModuleNamePs)
import PureScript.Backend.Erl.Syntax (ErlExpr)
import PureScript.Backend.Erl.Syntax as S
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal, ModuleName(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (BackendSemantics, EvalRef, ExternSpine(..), NeutralExpr(..))
import PureScript.Backend.Optimizer.Semantics as Sem
import PureScript.Backend.Optimizer.Semantics.Foreign (ForeignSemantics, qualified)
import PureScript.Backend.Optimizer.Syntax (BackendSyntax)
import PureScript.Backend.Optimizer.Syntax as Syn
import Safe.Coerce (coerce)
import Type.Equality (class TypeEquals, to, from)

data CWB :: (Type -> Type) -> Type -> Type -> Type
data CWB call b a =
  CWB b (call a)
derive instance functorCallWithBase :: Functor call => Functor (CWB call b)
derive instance bifunctorCallWithBase :: Functor call => Bifunctor (CWB call)

baseOf :: forall call b a. CWB call b a -> b
baseOf (CWB base _) = base

noBase :: forall call b a. CWB call b a -> call a
noBase (CWB _ call) = call

-- Different ways a function can be called in PureScript
data CallPS a
  = Curried (NonEmptyArray a)
  | Uncurried (Array a)
  | UncurriedEffect (Array a)
derive instance eqCallPS :: Eq a => Eq (CallPS a)
derive instance ordCallPS :: Ord a => Ord (CallPS a)
derive instance functorCallPS :: Functor CallPS
derive instance foldableCallPS :: Foldable CallPS
derive instance traversableCallPS :: Traversable CallPS
data CallingPS a
  = BasePS
  | CallingPS (CallingPS a) (CallPS a)
derive instance eqCallingPS :: Eq a => Eq (CallingPS a)
derive instance ordCallingPS :: Ord a => Ord (CallingPS a)
derive instance functorCallingPS :: Functor CallingPS
derive instance foldableCallingPS :: Foldable CallingPS
derive instance traversableCallingPS :: Traversable CallingPS

instance altCallingPS :: Alt CallingPS where
  alt calls BasePS = calls
  alt calls (CallingPS more call) = case CallingPS (alt calls more) call of
    CallingPS (CallingPS inner (Curried as)) (Curried bs) -> CallingPS inner (Curried (as <> bs))
    r -> r

callPS :: CallPS ~> CallingPS
callPS = CallingPS BasePS

type ArityPS = CallingPS Unit
type GlobalPS = Qualified Ident

headPS :: forall a. CallingPS a -> Maybe (CallPS a)
headPS BasePS = Nothing
headPS (CallingPS BasePS hd) = Just hd
headPS (CallingPS more _) = headPS more

setHeadPS :: forall a. CallPS a -> CallingPS a -> CallingPS a
setHeadPS hd BasePS = CallingPS BasePS hd
setHeadPS hd (CallingPS BasePS _) = CallingPS BasePS hd
setHeadPS hd (CallingPS more tail) = CallingPS (setHeadPS hd more) tail

dropHeadPS :: forall a. CallingPS a -> CallingPS a
dropHeadPS BasePS = BasePS
dropHeadPS (CallingPS BasePS _) = BasePS
dropHeadPS (CallingPS more tail) = CallingPS (dropHeadPS more) tail

canonicalize :: CallingPS ~> CallingPS
canonicalize (CallingPS (CallingPS more (Curried as)) (Curried bs)) =
  canonicalize (CallingPS more (Curried (as <> bs)))
canonicalize (CallingPS more as) = CallingPS (canonicalize more) as
canonicalize BasePS = BasePS

-- There is only one way a function can be called in Erlang
newtype CallErl a
  = Call (Array a)
derive instance newtypeCallErl :: Newtype (CallErl a) _
derive instance functorCallErl :: Functor CallErl
derive instance foldableCallErl :: Foldable CallErl
derive instance traversableCallErl :: Traversable CallErl
-- But a global definition cannot be called zero times
newtype CallingErl a = CallingErl (NonEmptyArray (CallErl a))
derive instance newtypeCallingErl :: Newtype (CallingErl a) _
instance altCallingErl :: Alt CallingErl where
  alt (CallingErl ls) (CallingErl rs) = CallingErl (alt ls rs)
derive instance functorCallingErl :: Functor CallingErl
derive instance foldableCallingErl :: Foldable CallingErl
derive instance traversableCallingErl :: Traversable CallingErl
type ArityErl = CallingErl Unit
newtype GlobalErl = GlobalErl { module :: Maybe String, name :: String }
derive instance newtypeGlobalErl :: Newtype GlobalErl _

callErl :: forall a. Array a -> CallingErl a
callErl = CallingErl <<< pure <<< Call

-- A map of calling conventions, from PS names and arities to global Erlang
-- functions and arities (the arities must match)
type Conventions =
  SemigroupMap (Qualified Ident)
  ( SemigroupMap ArityPS
    (Last (CWB CallingErl GlobalErl Unit))
  )
callAs :: Qualified Ident -> ArityPS -> ArityErl -> Conventions
callAs qi arity call =
  SemigroupMap $ Map.singleton qi $ SemigroupMap $ Map.singleton arity $ Last $
    CWB (toGlobalErl qi) call

toGlobalErl :: Qualified Ident -> GlobalErl
toGlobalErl (Qualified mmn (Ident ident)) = GlobalErl
  { module: mmn <#> erlModuleNamePs
  , name: ident
  }


conventionWithBase :: forall a b. (a -> b) -> CWB CallingPS (Qualified Ident) a -> CWB CallingErl GlobalErl b
conventionWithBase f (CWB qi call) =
  CWB
    (toGlobalErl qi)
    (globalConvention f call)

globalConvention :: forall a b. (a -> b) -> CallingPS a -> CallingErl b
globalConvention f = \calls -> go calls empty
  where
  go (CallingPS more call) acc =
    go more $ NEA.toArray (unwrap (callConvention f call)) <|> acc
  go BasePS acc =
    CallingErl (NEA.cons' (Call []) acc)

localConvention :: forall a b. (a -> b) -> CallingPS a -> Maybe (CallingErl b)
localConvention f = \calls -> go calls empty
  where
  go (CallingPS more call) acc =
    go more $ NEA.toArray (unwrap (callConvention f call)) <|> acc
  go BasePS acc =
    CallingErl <$> NEA.fromArray acc

callConvention :: forall a b. (a -> b) -> CallPS a -> CallingErl b
callConvention f = case _ of
  Curried calls -> CallingErl $ Call <<< Array.singleton <<< f <$> calls
  Uncurried call -> CallingErl $ NEA.singleton $ Call $ f <$> call
  UncurriedEffect call -> CallingErl $ NEA.singleton $ Call $ f <$> call

popPS :: forall a. CallingPS a -> Maybe { last :: a, init :: CallingPS a }
popPS BasePS = Nothing
popPS (CallingPS calls (Curried args)) =
  Just case NEA.unsnoc args of
    { last, init } | Just args' <- NEA.fromArray init ->
      { last, init: CallingPS calls (Curried args') }
    { last } ->
      { last, init: calls }
popPS (CallingPS calls (Uncurried args)) =
  case Array.unsnoc args of
    Just { last, init } ->
      Just { last, init: CallingPS calls (Uncurried init) }
    Nothing -> popPS calls
popPS (CallingPS calls (UncurriedEffect args)) =
  case Array.unsnoc args of
    Just { last, init } ->
      Just { last, init: CallingPS calls (UncurriedEffect init) }
    Nothing -> popPS calls

customConvention :: forall a b c. (a -> b -> c) -> CallingPS a -> CallingErl b -> Maybe (CallingErl c)
customConvention f available expected = do
  let
    popOne :: Maybe (CallingPS a) -> b -> { value :: Maybe c, accum :: Maybe (CallingPS a) }
    popOne s b = case s >>= popPS of
      Just { last, init } -> { value: Just (f last b), accum: Just init }
      Nothing -> { value: Nothing, accum: Nothing }
  case mapAccumR popOne (Just available) expected of
    { accum: Just remaining, value } -> do
      guard $ isNothing $ popPS remaining
      sequence value
    _ -> Nothing

class Calling :: (Type -> Type) -> Constraint
class Functor call <= Calling call where
  arity :: forall a. call a -> Int
  zipAgainst :: forall a b. call a -> call b -> Maybe { matched :: call (Tuple a b), unmatched :: Maybe (call a) }

instance Calling Array where
  arity = Array.length
  zipAgainst ls rs | Array.length ls == Array.length rs =
    Just { matched: Array.zip ls rs, unmatched: Nothing }
  zipAgainst _ _ = Nothing
instance Calling NonEmptyArray where
  arity = NEA.length
  zipAgainst ls rs | NEA.length ls >= NEA.length rs =
    Just { matched: NEA.zip ls rs, unmatched: NEA.fromArray (NEA.drop (NEA.length rs) ls) }
  zipAgainst _ _ = Nothing
instance (Calling call, Eq b) => Calling (CWB call b) where
  arity (CWB _ call) = arity call
  zipAgainst (CWB b ls) (CWB b' rs) | b == b' = do
    { matched, unmatched } <- zipAgainst ls rs
    Just { matched: CWB b' matched, unmatched: CWB b <$> unmatched }
  zipAgainst _ _ = Nothing

instance Calling CallPS where
  arity (Curried args) = NEA.length args
  arity (Uncurried args) = Array.length args
  arity (UncurriedEffect args) = Array.length args
  zipAgainst (Curried args) (Curried spec) = do
    { matched, unmatched } <- zipAgainst args spec
    Just { matched: Curried matched, unmatched: Curried <$> unmatched }
  zipAgainst (Uncurried args) (Uncurried spec) = do
    { matched, unmatched } <- zipAgainst args spec
    Just { matched: Uncurried matched, unmatched: Uncurried <$> unmatched }
  zipAgainst (UncurriedEffect args) (UncurriedEffect spec) = do
    { matched, unmatched } <- zipAgainst args spec
    Just { matched: UncurriedEffect matched, unmatched: UncurriedEffect <$> unmatched }
  zipAgainst _ _ = Nothing
instance Calling CallingPS where
  arity BasePS = 0
  arity (CallingPS calls call) = arity calls + arity call
  zipAgainst BasePS BasePS = Just { matched: BasePS, unmatched: Nothing }
  zipAgainst calls BasePS = Just { matched: BasePS, unmatched: Just calls }
  zipAgainst (CallingPS calls call) (CallingPS specs spec) = do
    { matched, unmatched } <- zipAgainst calls specs
    case unmatched >>= headPS of
      Just u1 -> do
        r <- zipAgainst u1 spec
        Just
          { matched: CallingPS matched r.matched
          , unmatched: case r.unmatched of
              Just u -> setHeadPS u <$> unmatched
              Nothing -> dropHeadPS <$> unmatched
          }
      Nothing -> do
        r <- zipAgainst call spec
        Just
          { matched: CallingPS matched r.matched
          , unmatched: case r.unmatched of
              Nothing -> Nothing
              Just u -> Just (CallingPS BasePS u)
          }
  zipAgainst BasePS (CallingPS _ _) = Nothing
instance Calling CallErl where
  arity (Call args) = Array.length args
  zipAgainst (Call args) (Call spec) =
    zipAgainst args spec >>= case _ of
      { matched, unmatched: Nothing } ->
        Just { matched: Call matched, unmatched: Nothing }
      _ -> Nothing
instance Calling CallingErl where
  arity (CallingErl calls) = sum (arity <$> calls)
  zipAgainst (CallingErl calls) (CallingErl specs) = do
    { matched, unmatched } <- zipAgainst calls specs
    matchedCalls <- for matched (uncurry zipExact)
    Just { matched: CallingErl matchedCalls, unmatched: CallingErl <$> unmatched }

zipExact :: forall call a b. Calling call => call a -> call b -> Maybe (call (Tuple a b))
zipExact l r = case zipAgainst l r of
  Just { matched, unmatched: Nothing } -> Just matched
  _ -> Nothing

zipEither :: forall call a b. Calling call => call a -> call b -> Maybe { matched :: call (Tuple a b), unmatched :: Maybe (Either (call a) (call b)) }
zipEither l r = case zipAgainst l r of
  Just { matched, unmatched } -> Just { matched, unmatched: Left <$> unmatched }
  Nothing -> case zipAgainst r l of
    Just { matched, unmatched } -> Just
      { matched: uncurry (flip Tuple) <$> matched, unmatched: Right <$> unmatched }
    Nothing -> Nothing

type MatchWithBase call expr base a =
  { matched :: CWB call base (Tuple expr a)
  , unmatched :: Maybe (call expr)
  }

class Calling call <= CallingExprBase call expr base env where
  -- Match a call against expr, returning some base and the other data matched
  -- along the way
  matchCall :: forall a. env -> expr -> call a -> Maybe (MatchWithBase call expr base a)
  -- Apply a call to base, returning a new expr
  applyCWB :: env -> CWB call base expr -> expr

applyCall :: forall env call expr base. CallingExprBase call expr base env => env -> base -> call expr -> expr
applyCall env base call = applyCWB env (CWB base call)

applyCallMaybe :: forall env call expr. CallingExprBase call expr expr env => env -> expr -> Maybe (call expr) -> expr
applyCallMaybe _ base Nothing = base
applyCallMaybe env base (Just call) = applyCall env base call

-- If the base is expr itself, then we can repeatedly apply some calls
applyCalls :: forall env call expr. CallingExprBase call expr expr env => env -> expr -> Array (call expr) -> expr
applyCalls env = foldl (applyCall env)

-- Call a global Erlang function (module name, function name)
instance CallingExprBase CallErl ErlExpr GlobalErl env where
  applyCWB _ (CWB (GlobalErl ref) (Call args)) =
    S.FunCall (S.Literal <<< S.Atom <$> ref.module) (S.Literal (S.Atom ref.name)) args
  matchCall _ (S.FunCall me (S.Literal (S.Atom name)) args) (Call argSpec)
    | Array.length args == Array.length argSpec = do
      mn <- case me of
        Nothing -> Just Nothing
        Just (S.Literal (S.Atom mn)) -> Just (Just mn)
        _ -> Nothing
      Just
        { matched: CWB (GlobalErl { module: mn, name }) (Call (Array.zip args argSpec))
        , unmatched: Nothing
        }
  matchCall _ _ _ = Nothing

-- Call a local function (some other expression, not global)
instance CallingExprBase CallErl ErlExpr ErlExpr env where
  applyCWB _ (CWB fn (Call args)) =
    S.FunCall Nothing fn args
  -- Exclude atoms
  matchCall _ (S.FunCall _ (S.Literal (S.Atom _)) _) _ = Nothing
  matchCall _ (S.FunCall Nothing fn args) (Call argSpec)
    | Array.length args == Array.length argSpec = Just
      { matched: CWB fn (Call (Array.zip args argSpec))
      , unmatched: Nothing
      }
  matchCall _ _ _ = Nothing

-- Iterated Erlang calls
instance CallingExprBase CallErl ErlExpr base env => CallingExprBase CallingErl ErlExpr base env where
  applyCWB env (CWB base (CallingErl calls)) =
    case NEA.uncons calls of
      { head, tail } ->
        applyCalls env (applyCWB env (CWB base head)) tail
  matchCall env top (CallingErl calls) =
    case NEA.uncons calls of
      { head, tail } ->
        -- We reverse the array into a stack of calls to match from right to
        -- left (outermost to innermost, where head is the most inner call)
        go head (Array.toUnfoldable (Array.reverse tail)) Nil top
    where
    -- Base case: we finally try matching the head call (leftmost)
    -- And return its base as our base
    go head Nil acc expr =
      matchCall env expr head >>= case _ of
        { matched: CWB base m, unmatched: Nothing } -> Just
          { matched: CWB base (CallingErl (consCall m acc))
          , unmatched: Nothing
          }
        _ -> Nothing
    -- Cons case: we peel away callSpec (the rightmost call, since the array was
    -- reversed into the list), match it against whatever the current call is,
    -- and use its base to keep matching the rest
    go head (Cons callSpec stack) acc expr =
      matchCall env expr callSpec >>= case _ of
        { matched: CWB expr' call, unmatched: Nothing } ->
          go head stack (Cons call acc) expr'
        _ -> Nothing
    -- At the end we have a single base call and a stack of calls
    consCall baseCall acc = NEA.cons' baseCall (Array.fromFoldable acc)

-- PureScript calls on BackendSyntax
instance
  ( Newtype s (BackendSyntax s)
  , Inj base (BackendSyntax s)
  ) => CallingExprBase CallPS (BackendSyntax s) base env where
  applyCWB _ (CWB expr (Curried args)) =
    Syn.App (wrap (review inj expr)) (wrap <$> args)
  applyCWB _ (CWB expr (Uncurried args)) =
    Syn.UncurriedApp (wrap (review inj expr)) (wrap <$> args)
  applyCWB _ (CWB expr (UncurriedEffect args)) =
    Syn.UncurriedEffectApp (wrap (review inj expr)) (wrap <$> args)
  matchCall _ (Syn.App fn call) (Curried callSpec) | NEA.length call >= NEA.length callSpec = do
    base <- preview inj (unwrap fn)
    Just
      { matched: CWB base (Curried (NEA.zip (coerce call) callSpec))
      , unmatched: NEA.fromArray (NEA.drop (NEA.length callSpec) call) <#> coerce >>> Curried
      }
  matchCall _ (Syn.UncurriedApp fn call) (Uncurried callSpec) | Array.length call == Array.length callSpec = do
    base <- preview inj (unwrap fn)
    Just
      { matched: CWB base (Uncurried (Array.zip (coerce call) callSpec))
      , unmatched: Nothing
      }
  matchCall _ (Syn.UncurriedEffectApp fn call) (UncurriedEffect callSpec) | Array.length call == Array.length callSpec = do
    base <- preview inj (unwrap fn)
    Just
      { matched: CWB base (UncurriedEffect (Array.zip (coerce call) callSpec))
      , unmatched: Nothing
      }
  matchCall _ _ _ = Nothing


instance
  ( Inj base NeutralExpr
  ) => CallingExprBase CallPS NeutralExpr base env where
    applyCWB env (CWB base call) = NeutralExpr $
      applyCWB env (CWB (un NeutralExpr (review inj base)) (un NeutralExpr <$> call))
    matchCall env expr spec = do
      { matched: CWB base call, unmatched } <- matchCall env (un NeutralExpr expr) spec
      base' <- preview inj (NeutralExpr base)
      pure
        { matched: CWB base' (coerce call)
        , unmatched: coerce unmatched
        }

fromEvalRef :: EvalRef -> BackendSemantics
fromEvalRef r =
  let ret = Sem.SemRef r [] (defer \_ -> ret) in ret

instance
  ( TypeEquals BackendSemantics base
  ) => CallingExprBase CallPS BackendSemantics base Sem.Env where
  applyCWB env (CWB expr (Curried args)) =
    Sem.evalApp env (from expr) (NEA.toArray args)
  applyCWB env (CWB expr (Uncurried args)) =
    Sem.evalUncurriedApp env (from expr) args
  applyCWB env (CWB expr (UncurriedEffect args)) =
    Sem.evalUncurriedEffectApp env (from expr) args
  matchCall = matchCall' false
    where
    matchCall' _ _ (Sem.SemRef r [Sem.ExternApp call'] _) (Curried callSpec)
      | Just call <- NEA.fromArray call'
      , NEA.length call >= NEA.length callSpec =
      Just
        { matched: CWB (to (fromEvalRef r)) (Curried (NEA.zip call callSpec))
        , unmatched: NEA.fromArray (NEA.drop (NEA.length callSpec) call) <#> Curried
        }
    matchCall' _ _ (Sem.SemRef r [Sem.ExternUncurriedApp call] _) (Uncurried callSpec)
      | Array.length call == Array.length callSpec =
        Just
          { matched: CWB (to (fromEvalRef r)) (Uncurried (Array.zip call callSpec))
          , unmatched: Nothing
          }
    -- -- There is no dedicated ExternUncurriedEffectApp
    -- matchCall' _ _ (Sem.SemRef r [Sem.ExternUncurriedApp call] _) (UncurriedEffect callSpec)
    --   | Array.length call == Array.length callSpec =
    --     Just
    --       { matched: CWB (to (fromEvalRef r)) (Uncurried (Array.zip call callSpec))
    --       , unmatched: Nothing
    --       }
    matchCall' _ env (Sem.NeutApp fn []) spec = matchCall env fn spec
    matchCall' _ _ (Sem.NeutApp fn call') (Curried callSpec)
      | Just call <- NEA.fromArray call'
      , NEA.length call >= NEA.length callSpec =
      Just
        { matched: CWB (to fn) (Curried (NEA.zip call callSpec))
        , unmatched: NEA.fromArray (NEA.drop (NEA.length callSpec) call) <#> Curried
        }
    matchCall' _ _ (Sem.NeutUncurriedApp fn call) (Uncurried callSpec) | Array.length call == Array.length callSpec =
      Just
        { matched: CWB (to fn) (Uncurried (Array.zip call callSpec))
        , unmatched: Nothing
        }
    matchCall' _ _ (Sem.NeutUncurriedEffectApp fn call) (UncurriedEffect callSpec) | Array.length call == Array.length callSpec =
      Just
        { matched: CWB (to fn) (UncurriedEffect (Array.zip call callSpec))
        , unmatched: Nothing
        }
    matchCall' true env (Sem.SemRef _ _ more) spec =
      matchCall' false env (force more) spec
    matchCall' _ _ _ _ = Nothing

class Inj a b where
  inj :: Prism' b a

instance Inj (Qualified Ident) BackendSemantics where
  inj = prism' (fromEvalRef <<< Sem.EvalExtern) case _ of
    Sem.SemRef (Sem.EvalExtern qi') [] _ -> Just qi'
    _ -> Nothing
else instance Inj (Literal BackendSemantics) BackendSemantics where
  inj = prism' Sem.NeutLit case _ of
    Sem.NeutLit l -> Just l
    _ -> Nothing
else instance Inj (Qualified Ident) NeutralExpr where
  inj = prism' (NeutralExpr <<< Syn.Var) case _ of
    NeutralExpr (Syn.Var qi') -> Just qi'
    _ -> Nothing
else instance Inj a a where
  inj = identity

instance
  ( CallingExprBase CallPS expr expr env
  , Inj base expr
  ) => CallingExprBase CallingPS expr base env where
  applyCWB _ (CWB base BasePS) = review inj base
  applyCWB env (CWB base (CallingPS calls call)) =
    applyCWB env (CWB (applyCWB env (CWB base calls)) call)
  matchCall _ base BasePS = preview inj base <#> \base' ->
    { matched: CWB base' BasePS
    , unmatched: Nothing
    }
  matchCall env expr (CallingPS callSpecs callSpec) = do
    { matched: CWB mid call, unmatched } <- matchCall env expr callSpec
    case matchCall env mid callSpecs of
      Just { matched: CWB base calls, unmatched: Nothing } -> Just
        { matched: CWB base (CallingPS calls call)
        , unmatched: unmatched <#> CallingPS BasePS
        }
      _ -> Nothing




class Functor call <= Structure call where
  hasEmpty :: forall a. Maybe (call a)
  hasOne :: forall a. Prism' (call a) a

instance Structure Array where
  hasEmpty = Just []
  hasOne = prism' pure case _ of
    [o] -> Just o
    _ -> Nothing
instance Structure NonEmptyArray where
  hasEmpty = Nothing
  hasOne = prism' pure case _ of
    os | [o] <- NEA.toArray os -> Just o
    _ -> Nothing
instance Structure CallingPS where
  hasEmpty = Just BasePS
  hasOne = prism' (\o -> CallingPS BasePS (Curried (pure o))) case _ of
    CallingPS BasePS (Curried os) | [o] <- NEA.toArray os -> Just o
    _ -> Nothing

_callPS :: forall a. Prism' (CallingPS a) (CallPS a)
_callPS = prism' callPS case _ of
  CallingPS BasePS r -> Just r
  _ -> Nothing
_curried :: forall a. Prism' (CallPS a) (NonEmptyArray a)
_curried = prism' Curried case _ of
  Curried r -> Just r
  _ -> Nothing
_uncurried :: forall a. Prism' (CallPS a) (Array a)
_uncurried = prism' Uncurried case _ of
  Uncurried r -> Just r
  _ -> Nothing
_uncurriedEffect :: forall a. Prism' (CallPS a) (Array a)
_uncurriedEffect = prism' UncurriedEffect case _ of
  UncurriedEffect r -> Just r
  _ -> Nothing


data Pattern env call i o = Pure (env -> Maybe o) | Pattern (call Unit) (env -> call i -> Maybe o)

shapeOf :: forall env call i o. Pattern env call i o -> call Unit
shapeOf (Pure _) = unsafeCrashWith "Pure patterns have no shape"
shapeOf (Pattern shape _) = shape

match :: forall env call i o. Calling call => env -> call i -> Pattern env call i o -> Maybe { result :: o, unmatched :: Maybe (call i) }
match env unmatched (Pure o) = o env <#>
  { result: _
  , unmatched: case arity unmatched of
      0 -> Nothing
      _ -> Just unmatched
  }
match env inputs (Pattern shape parse) = do
  { matched, unmatched } <- zipAgainst inputs shape
  result <- parse env (map fst matched)
  pure { result, unmatched }

match' :: forall env call b i o. Eq b => CallingExprBase call i b env => env -> i -> Pattern env (CWB call b) i o -> Maybe o
match' _env _unmatched (Pure _o) = Nothing
match' env input (Pattern (CWB base shape) parse) = do
  { matched, unmatched } <- matchCall env input shape
  guard $ base == baseOf matched
  guard $ isNothing unmatched
  parse env (map fst matched)

match'' :: forall env call b i o. CallingExprBase call i b env => env -> i -> Pattern env (CWB call b) i o -> Maybe o
match'' _env _unmatched (Pure _o) = Nothing
match'' env input (Pattern (CWB _ shape) parse) = do
  { matched, unmatched } <- matchCall env input shape
  guard $ isNothing unmatched
  parse env (map fst matched)

derive instance profunctorPattern :: Functor call => Profunctor (Pattern env call)
derive instance functorPattern :: Functor call => Functor (Pattern env call i)
instance applyPattern :: (Calling call, Alt call) => Apply (Pattern env call i) where
  apply (Pure f) (Pure a) = Pure (lift2 apply f a)
  apply (Pure f) as = withEnv' ((\a env -> f env <@> a) <$> as)
  apply fs (Pure a) = withEnv' ((\f env -> f <$> a env) <$> fs)
  apply (Pattern patL parseL) (Pattern patR parseR) =
    Pattern (patL <|> patR) \env full -> do
      rL <- zipAgainst full patL
      rest <- rL.unmatched
      rR <- zipAgainst rest patR
      guard $ isNothing rR.unmatched
      parseL env (map fst rL.matched) <*> parseR env (map fst rR.matched)
instance applicativePattern :: (Calling call, Alt call) => Applicative (Pattern env call i) where
  pure = Pure <<< pure <<< pure

instance compactablePattern :: Functor call => Compactable (Pattern env call i) where
  compact (Pure f) = Pure $ f >>> compact
  compact (Pattern shape parser) = Pattern shape \env inputs ->
    join $ parser env inputs
  separate x = separateDefault x

instance filterablePattern :: Functor call => Filterable (Pattern env call i) where
  filterMap f = map f >>> compact
  partitionMap x = partitionMapDefault x
  partition x = partitionDefault x
  filter x = filterDefault x

arg :: forall env call i. Structure call => Pattern env call i i
arg = arg' identity

_absorb :: forall a b. (Partial => a -> Maybe b) -> a -> Maybe b
_absorb f i = unsafePerformEffect $ catchException (const (pure Nothing)) do
  pure unit
  pure (unsafePartial f i)

partial :: forall env call i o. (Partial => Pattern env call i o) -> Pattern env call i o
partial pat = unsafePartial pat

arg' :: forall env call i o. Structure call => (Partial => i -> o) -> Pattern env call i o
arg' f = Pattern (review hasOne unit) \_env -> preview hasOne >=> _absorb \i -> Just (f i)

argMatch :: forall env call1 call2 b i o. Eq b => Structure call2 => CallingExprBase call1 i b env => Pattern env (CWB call1 b) i o -> Pattern env call2 i o
argMatch pat = Pattern (review hasOne unit) \env -> preview hasOne >=> match' env <@> pat

many :: forall f env call i o. Traversable f => Alt call => Calling call => Structure call => f (i -> o) -> Pattern env call i (f o)
many fs = traverse (\f -> arg' f) fs

noArgs :: forall env i. Pattern env CallingPS i Unit
noArgs = Pattern BasePS \_ _ -> Just unit

codegenArg :: forall call i o. Structure call => Pattern (i -> o) call i o
codegenArg = withEnv (map (#) arg)

func :: forall env i o. String -> Pattern env CallingPS i o -> Pattern env (CWB CallingPS (Qualified Ident)) i o
func _ (Pure o) = Pure o
func name (Pattern shape parser) =
  let qi = qualPS name in
  Pattern (CWB qi shape) \env -> case _ of
    CWB qi' inputs | qi == qi' -> parser env inputs
    _ -> Nothing

getEnv :: forall env call i. Calling call => Alt call => Pattern env call i env
getEnv = withEnv (pure identity)

withEnv :: forall env call i o. Pattern env call i (env -> o) -> Pattern env call i o
withEnv (Pure o) = Pure \env -> o env <@> env
withEnv (Pattern shape parser) = Pattern shape \env inputs -> parser env inputs <@> env

withEnv' :: forall env call i o. Pattern env call i (env -> Maybe o) -> Pattern env call i o
withEnv' (Pure o) = Pure \env -> join (o env <@> env)
withEnv' (Pattern shape parser) = Pattern shape \env inputs -> join (parser env inputs <@> env)

lift :: forall env call1 call2 i o. (forall a. APrism' (call2 a) (call1 a)) -> Pattern env call1 i o -> Pattern env call2 i o
lift _ (Pure o) = Pure o
lift p (Pattern shape parse) =
  withPrism p \rev _ ->
    Pattern (rev shape) \env ->
      withPrism p \_ pre ->
        pre >>> hush >=> parse env

curried :: forall env i o. Pattern env NonEmptyArray i o -> Pattern env CallingPS i o
curried = lift (_callPS <<< _curried)

uncurried :: forall env i o. Pattern env Array i o -> Pattern env CallingPS i o
uncurried = lift (_callPS <<< _uncurried)

uncurriedEffect :: forall env i o. Pattern env Array i o -> Pattern env CallingPS i o
uncurriedEffect (Pure o) = Pure o
uncurriedEffect (Pattern shape parse) =
  Pattern (CallingPS BasePS (UncurriedEffect shape)) \env -> case _ of
    CallingPS BasePS (UncurriedEffect is) | Array.length is == Array.length shape ->
      parse env is
    _ -> Nothing

qualPS :: String -> Qualified Ident
qualPS qi = case String.split (String.Pattern ".") qi # Array.unsnoc of
  Just { init, last } | Array.length init > 0, not String.contains (String.Pattern ":") qi ->
    qualified (String.joinWith "." init) last
  _ -> unsafeCrashWith $ "Bad PureScript qualified ident: " <> show qi

qualErl :: String -> GlobalErl
qualErl qi = case String.split (String.Pattern ":") qi of
  [ moduleName, name ] | not String.contains (String.Pattern ".") moduleName -> GlobalErl
    { module: Just moduleName, name }
  _ -> unsafeCrashWith $ "Bad Erlang qualified ident: " <> show qi

ofExternSpine :: Array Sem.ExternSpine -> { converted :: CallingPS BackendSemantics, unconverted :: Array ExternSpine }
ofExternSpine = Array.toUnfoldable >>> go BasePS
  where
  go acc (Cons (ExternApp args') more) | Just args <- NEA.fromArray args' =
    go (CallingPS acc (Curried args)) more
  go acc (Cons (ExternUncurriedApp args) more) =
    go (CallingPS acc (Uncurried args)) more
  go converted unconverted =
    { converted
    , unconverted: List.toUnfoldable unconverted
    }

evaluator :: Pattern Sem.Env (CWB CallingPS (Qualified Ident)) BackendSemantics BackendSemantics -> ForeignSemantics
evaluator pat = Tuple (baseOf (shapeOf pat)) \env qual spine -> do
  { converted, unconverted } <- Just (ofExternSpine spine)
  { result, unmatched } <- match env (CWB qual converted) pat
  pure (Sem.evalSpine env (applyCallMaybe env result (noBase <$> unmatched)) unconverted)

evaluators :: String -> Array (Pattern Sem.Env CallingPS BackendSemantics BackendSemantics) -> ForeignSemantics
evaluators name pats =
  Tuple (qualPS name) \env _qual spine -> pats # Array.findMap \pat -> do
    { converted, unconverted } <- Just (ofExternSpine spine)
    { result, unmatched } <- match env converted pat
    pure (Sem.evalSpine env (applyCallMaybe env result unmatched) unconverted)

popTrivial :: forall a. CallingPS a -> Tuple (CallingPS a) (CallingPS Void)
popTrivial (CallingPS a (Uncurried [])) = CallingPS <$> popTrivial a <@> Uncurried []
popTrivial (CallingPS a (UncurriedEffect [])) = CallingPS <$> popTrivial a <@> UncurriedEffect []
popTrivial a = Tuple a BasePS

abstractTrivial :: ErlExpr -> CallingPS Void -> ErlExpr
abstractTrivial e BasePS = e
abstractTrivial e (CallingPS more _) = S.thunk (abstractTrivial e more)

callConverters :: Array { ps :: Qualified Ident, arity :: ArityPS, erl :: GlobalErl, call :: ArityErl } -> (NeutralExpr -> ErlExpr) -> NeutralExpr -> Maybe ErlExpr
callConverters options codegenExpr focus = options # Array.findMap \option -> do
  case arity option.arity == arity option.call of
    true -> pure unit
    false -> unsafeCrashWith $ "Arity of call did not match"
  -- `option.arity` may come with some thunks, that we do not want to require
  -- to complete a match. so we pop these off:
  let Tuple required trivial = popTrivial option.arity
  -- and match the actual required bits:
  { matched: CWB base matched, unmatched: unmatched1 } <- matchCall unit focus required
  guard $ base == option.ps
  -- we know how to codegen the matched bits
  calls <- customConvention (fst >>> codegenExpr >>> const) matched option.call
  let converted = applyCall unit option.erl calls
  -- then we check what was unmatched from the first step against what was trivial:
  { matched: _handled :: _ (Tuple _ Void), unmatched: unmatchedEither } <- zipEither (fromMaybe BasePS unmatched1) trivial
  -- then we might have unmatched bits left over, or trivial bits that did not match
  case unmatchedEither of
    Nothing -> pure converted
    Just (Left unmatched) -> do
      unmatched' <- localConvention codegenExpr unmatched
      pure $ applyCall unit converted unmatched'
    Just (Right trivial) -> do
      pure $ abstractTrivial converted trivial

applyConventions :: Conventions -> (NeutralExpr -> ErlExpr) -> NeutralExpr -> Maybe ErlExpr
applyConventions conventions = callConverters $
  conventions # foldMapWithIndex \ps -> foldMapWithIndex \arity (Last (CWB erl call)) ->
    pure { ps, arity, erl, call }


type Converter =
  Pattern (NeutralExpr -> ErlExpr) (CWB CallingPS (Qualified Ident)) NeutralExpr ErlExpr

convert :: Converter -> (NeutralExpr -> ErlExpr) -> NeutralExpr -> Maybe ErlExpr
convert pat codegenExpr expr = do
  match' codegenExpr expr pat

-- TODO: organize a map by identifier
converts :: Array Converter -> (NeutralExpr -> ErlExpr) -> NeutralExpr -> Maybe ErlExpr
converts pats codegenExpr expr = pats # Array.findMap \pat ->
  match' codegenExpr expr pat

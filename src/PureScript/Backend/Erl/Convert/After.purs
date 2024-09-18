-- | This runs after the PureScript gets converted to Erlang. It handles several
-- | aspects to produce better Erlang. It optimizes pattern matching in two passes
-- | and it calls `Scoping` (one pass, twice) to convert the PureScript-y variable
-- | scoping into Erlang variable scoping (where variables can leak out of cases).
-- |
-- | The two passes here:
-- |   - `optimizePatternDemand` analyzes demand for fields of variables and
-- |     inserts patterns to take advantage of that. It also hoists global pure
-- |     calls and turns `if`s into `case`s.
-- |   - `optimizePatternRewrite` inlines those added patterns, so variables
-- |     are referenced directly. It also does beta-reduction of simple
-- |     functions directly applied to their arguments and hoists arguments
-- |     to macros (since macros are fussy).
module PureScript.Backend.Erl.Convert.After where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Apply (lift2)
import Control.Bind (bindFlipped)
import Control.Monad.Writer (WriterT(..), censor, runWriterT, tell)
import Control.Plus (empty)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Bitraversable (bitraverse)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Functor.Compose (Compose(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity(..))
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Endo (Endo(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Newtype (over, unwrap)
import Data.String as String
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, for, mapAccumL, sequence, sum, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Coerce (class Coercible)
import PureScript.Backend.Erl.Calling (GlobalErl(..), applyCall, callErl)
import PureScript.Backend.Erl.Convert.Scoping (renameRoot)
import PureScript.Backend.Erl.Syntax (Accessor(..), Accessors, CaseClause(..), ErlDefinition(..), ErlExpr(..), ErlPattern, FunHead(..), Guard(..), IfClause(..), access, self, shortCircuits)
import PureScript.Backend.Erl.Syntax as S
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

-- Demand analysis for variables, nested through records and ADTs.
data Demand
  = Unreachable
  | NoDemand
  -- A basic count of demand
  | Demand Int
  -- The demand for the gestalt specifically
  -- If we know the constructor, tagname and arity
  -- And then a map of demand for each field (indexed by ??)
  | DemandADT Int (Maybe (Tuple String Int)) (Map Int Demand)
  -- The demand for the gestalt specifically
  -- Map of demand for each field
  | DemandRecord Int (Map String Demand)

-- | `add` handles combining demands across cases, `mul` handles combining
-- | demands through sequential code
-- |
-- | Related to the tropical semiring (max, +)
instance semiringDemand :: Semiring Demand where
  zero = Unreachable
  one = NoDemand

  add Unreachable d = d
  add d Unreachable = d
  add NoDemand _ = NoDemand
  add _ NoDemand = NoDemand
  add (Demand u1) (Demand u2) = Demand (max u1 u2)
  add (Demand u1) (DemandADT u2 _ _) = Demand (max u1 u2)
  add (DemandADT u1 _ _) (Demand u2) = Demand (max u1 u2)
  add (Demand u1) (DemandRecord u2 _) = Demand (max u1 u2)
  add (DemandRecord u1 _) (Demand u2) = Demand (max u1 u2)
  add (DemandADT u1 l1 d1) (DemandADT u2 l2 d2)
    -- We only take them if they agree on what constructor
    | Just l <- agreed l1 l2 =
    DemandADT (max u1 u2) l (Map.intersectionWith add d1 d2)
  add (DemandRecord u1 d1) (DemandRecord u2 d2) =
    DemandRecord (max u1 u2) (Map.intersectionWith add d1 d2)
  add _ _ = NoDemand

  mul NoDemand d = d
  mul d NoDemand = d
  mul Unreachable _ = Unreachable
  mul _ Unreachable = Unreachable
  mul (Demand u1) (Demand u2) = Demand (u1 + u2)
  mul (Demand u1) (DemandADT u2 l d) = DemandADT (u1 + u2) l d
  mul (DemandADT u1 l d) (Demand u2) = DemandADT (u1 + u2) l d
  mul (Demand u1) (DemandRecord u2 d) = DemandRecord (u1 + u2) d
  mul (DemandRecord u1 d) (Demand u2) = DemandRecord (u1 + u2) d
  mul (DemandADT u1 l1 d1) (DemandADT u2 l2 d2)
    -- We're in trouble if they disagree on the constructor; but if only one
    -- of them knows, that is fine
    | Just l <- common l1 l2 =
    DemandADT (u1 + u2) l (Map.unionWith mul d1 d2)
  mul (DemandRecord u1 d1) (DemandRecord u2 d2) =
    DemandRecord (u1 + u2) (Map.unionWith mul d1 d2)
  mul _ _ = Unreachable

agreed :: forall a. Eq a => Maybe a -> Maybe a -> Maybe (Maybe a)
agreed (Just a) (Just b) | a == b = Just (Just a)
agreed Nothing Nothing = Just Nothing
agreed _ _ = Nothing

common :: forall a. Eq a => Maybe a -> Maybe a -> Maybe (Maybe a)
common (Just a) (Just b) | a /= b = Nothing
common a b = Just (a <|> b)

-- | We keep track of demands for pure Erlang functions and each local variable.
data Demands
  = Demands (Map GlobalErl Demand) (Map String Demand)
  | NullAndVoid

-- | Lookup demand of a variable.
demandOf :: forall d. Coercible d Demands => String -> d -> Maybe Demand
demandOf n = coerce case _ of
  Demands _ m -> Map.lookup n m
  NullAndVoid -> Nothing

-- | Type synonym for tracking demand through a single branch of control flow.
type MDemands = Multiplicative Demands
-- | Type synonym for tracking demand through separate cases.
type ADemands = Additive Demands

instance semiringDemands :: Semiring Demands where
  zero = NullAndVoid
  one = Demands Map.empty Map.empty

  add (Demands g1 d1) (Demands g2 d2) = Demands
    -- Add demands for variables across cases, and `NoDemand` for variables
    -- missing in one case.
    (Map.union (lift2 add g1 g2) (NoDemand <$ (g1 <|> g2)))
    (Map.union (lift2 add d1 d2) (NoDemand <$ (d1 <|> d2)))
  add d@(Demands _ _) NullAndVoid = d
  add NullAndVoid d@(Demands _ _) = d
  add NullAndVoid NullAndVoid = NullAndVoid
  mul (Demands g1 d1) (Demands g2 d2) = Demands
    (Map.unionWith mul g1 g2)
    (Map.unionWith mul d1 d2)
  mul _ _ = NullAndVoid

-- | Run optimizations on `ErlDefinition`.
optimizePatternsDecl :: ErlDefinition -> ErlDefinition
-- TODO this is really ugly
optimizePatternsDecl (FunctionDefinition name args (S.Macro "MEMOIZE_AS" (Just keyExpr))) | [ key, metadata, expr ] <- NEA.toArray keyExpr =
  case optimizePatternsDecl (FunctionDefinition name args expr) of
    FunctionDefinition _ args' expr' ->
      FunctionDefinition name args' (S.Macro "MEMOIZE_AS" (Just (NEA.cons' key [ metadata, expr' ])))
optimizePatternsDecl (FunctionDefinition name args expr) =
  -- Run it as a function definition.
  case optimizePatterns (S.Fun Nothing [ Tuple (FunHead args Nothing) expr ]) of
    S.Fun Nothing [ Tuple (FunHead args' Nothing) expr' ] ->
      FunctionDefinition name args' expr'
    _ -> unsafeCrashWith "Did not rewrite to function of right shape in optimizePatternsDecl"

-- | Optimize the Erlang: optimize patterns in two passes and do renaming.
optimizePatterns :: ErlExpr -> ErlExpr
optimizePatterns = identity
    >>> optimizePatternDemand
    >>> flip runWriterT mempty
    >>> (\(Tuple e _) -> optimizePatternRewrite e mempty)
    >>> renameRoot
    -- Run it twice since otherwise there are gaps in the chosen numbered
    -- variables from variables that did not end up being necessary
    >>> renameRoot

opd :: ErlExpr -> W ErlExpr
opd e = optimizePatternDemand e
opds :: forall f. Traversable f => f ErlExpr -> W (f ErlExpr)
opds es = traverse opd es
opdss :: forall f g. Traversable f => Traversable g => f (g ErlExpr) -> W (f (g ErlExpr))
opdss ess = traverse (traverse opd) ess
opdg :: Maybe Guard -> W (Maybe Guard)
opdg mg = traverse (coerce opd) mg

-- | Use demand analysis to supplement patterns.
choosePattern :: MDemands -> ErlPattern -> ErlPattern
choosePattern (Multiplicative (Demands _ m)) = case _ of
  S.BindVar v | Just d <- Map.lookup v m -> do
    v `andMatch` flatPat (makePatterns v NoDemand d)
  p -> p
choosePattern _ = identity

flatPat :: Array (Tuple ErlPattern ErlExpr) -> ErlPattern
flatPat [Tuple pat _] = pat
flatPat _ = S.Discard

-- | Optimized `MatchBoth` / `BindVar`
andMatch :: String -> ErlPattern -> ErlPattern
andMatch v S.Discard = S.BindVar v
andMatch v pat = S.MatchBoth v pat

-- | Analyze demand to add patterns.
-- |
-- | Note that this invents strange variable names that are almost surely
-- | not valid Erlang, so it does really need to be followed by `renameRoot`
-- | at some point.
makePatternsFrom :: String -> Tuple String Accessors -> Demand -> Demand -> Array (Tuple ErlPattern ErlExpr)
makePatternsFrom _ _ _ (DemandRecord _ fields) | Map.isEmpty fields = []
makePatternsFrom v (Tuple v0 acsrs) (DemandRecord _ prev) (DemandRecord _ fields) =
  Map.toUnfoldable fields # Array.mapMaybe Just # foldMapWithIndex \i (Tuple k demand) -> do
    -- Sorry
    let v' = v <> "@@" <> String.replaceAll (String.Pattern "@@") (String.Replacement "@@@") k
    -- let v' = v <> "@@" <> show (i + 1 + Map.size prev)
    makePatternsFrom v' (Tuple v0 (acsrs <> [AcsKey k])) (fromMaybe NoDemand $ Map.lookup k prev) demand
makePatternsFrom v (Tuple v0 acsrs) prev (DemandRecord _ fields) =
  pure $ flip Tuple (S.Var v0 acsrs) $ S.MatchMap $
    Map.toUnfoldable fields # mapWithIndex \i (Tuple k demand) -> do
      -- Sorry
      let v' = v <> "@@" <> String.replaceAll (String.Pattern "@@") (String.Replacement "@@@") k
      -- let v' = v <> "@@" <> show i
      Tuple k (andMatch v' (flatPat (makePatternsFrom v' (Tuple v0 (acsrs <> [AcsKey k])) NoDemand demand)))
makePatternsFrom _ _ _ (DemandADT _ (Just (Tuple _ 0)) _) = []
makePatternsFrom v (Tuple v0 acsrs) (DemandADT _ (Just t1) prev) (DemandADT _ (Just t2@(Tuple tag arity)) fields)
  | t1 == t2 =
    Array.replicate arity unit # foldMapWithIndex \i _ ->
      Map.lookup (i+2) fields # foldMap \demand -> do
        let v' = v <> "@@" <> show i
        makePatternsFrom v' (Tuple v0 (acsrs <> [AcsElement (i+2)])) (fromMaybe NoDemand $ Map.lookup (i+2) prev) demand
makePatternsFrom v (Tuple v0 acsrs) prev (DemandADT _ (Just (Tuple tag arity)) fields) =
  pure $ flip Tuple (S.Var v0 acsrs) $ S.MatchTuple $
    [S.MatchLiteral (S.Atom tag)] <> do
      Array.replicate arity unit # mapWithIndex \i _ -> case Map.lookup (i+2) fields of
        Nothing -> S.Discard
        Just demand -> do
          let v' = v <> "@@" <> show i
          andMatch v' (flatPat (makePatternsFrom v' (Tuple v0 (acsrs <> [AcsElement (i+2)])) NoDemand demand))
makePatternsFrom _ _ _ _ = []

makePatterns :: String -> Demand -> Demand -> Array (Tuple ErlPattern ErlExpr)
makePatterns v = makePatternsFrom v (Tuple v self)

demandAccessors :: Accessors -> Demand
demandAccessors = demandAccessors' <@> Demand 1

-- | Embed demand through an accessor.
demandAccessors' :: Accessors -> Demand -> Demand
demandAccessors' =
  flip $ foldr $
    flip case _, _ of
      -- Remember that the first field is the demand on the variable
      -- specifically, so it is zero here
      d, AcsElement idx -> DemandADT 0 Nothing $ Map.singleton idx d
      d, AcsKey key -> DemandRecord 0 $ Map.singleton key d
      -- We do not optimize the other accessors, so they revert to plain
      -- `Demand`
      d, _ -> Demand $ allDemand d

-- | Count up all the demand (on accessors too).
allDemand :: Demand -> Int
allDemand (Demand i) = i
allDemand (DemandADT i _ ds) = i + sum (allDemand <$> ds)
allDemand (DemandRecord i ds) = i + sum (allDemand <$> ds)
allDemand NoDemand = 0
allDemand Unreachable = 0

-- | The immediate demand on the variable, not its children.
selfDemand :: Demand -> Int
selfDemand (Demand i) = i
selfDemand (DemandADT i _ _) = i
selfDemand (DemandRecord i _) = i
selfDemand NoDemand = 0
selfDemand Unreachable = 0

-- | The transitive demand on the variable's children, not itself.
childDemand :: Demand -> Int
childDemand = allDemand - selfDemand

-- | Unused but nice for debugging.
justDemands :: MDemands -> MDemands
justDemands = over Multiplicative case _ of
  NullAndVoid -> NullAndVoid
  Demands g m -> Demands g $ m <#> allDemand >>> Demand

addedDemands :: MDemands -> MDemands -> Array (Tuple ErlPattern ErlExpr)
addedDemands (Multiplicative (Demands _ d1)) (Multiplicative (Demands _ d2)) =
  bindFlipped (\(Tuple v f) -> f v >>= addBinding) $ Map.toUnfoldable $ lift2 addedDemand d1 d2
  where
  -- Really this should look at the demand of the stuff getting added to the
  -- pattern ...
  -- addedDemand _ d _ | childDemand d < 2 = empty
  addedDemand _ NoDemand _ = empty
  addedDemand _ (Demand _) _ = empty
  addedDemand prev next v = makePatterns v prev next

  addBinding (Tuple pat expr) = case pat of
    S.Discard -> empty
    S.BindVar _ -> empty
    S.MatchBoth _ S.Discard -> empty
    S.MatchBoth _ (S.BindVar _) -> empty
    _ -> pure $ Tuple pat expr
addedDemands _ _ = []

addedGlobalDemands :: MDemands -> MDemands -> Array (Tuple ErlPattern ErlExpr)
addedGlobalDemands (Multiplicative (Demands g1 _)) (Multiplicative (Demands g2 _)) =
  Array.fromFoldable $ Map.mapMaybeWithKey (#) $ lift2 addedDemand g1 g2
  where
  addedDemand (Demand prev) (Demand next) k | next > 1 =
    Just $ Tuple (S.BindVar $ "V@@@" <> show (unwrap k)) $
      applyCall unit k (callErl [])
  addedDemand _ _ _ = Nothing
addedGlobalDemands _ _ = []

optimizeDemands :: MDemands -> Tuple MDemands ErlExpr -> ErlExpr
optimizeDemands agg (Tuple this e) =
  case addedDemands agg this <> addedGlobalDemands agg this of
    [] -> e
    asgns -> Assignments (Array.reverse asgns) e

-- | Handle control flow of separate branches.
alts ::
  forall f a b.
    Traversable f =>
  f a ->
  (a -> W (Tuple (Array ErlPattern) (MDemands -> MDemands -> b))) ->
  W (f b)
alts cases mapper = WriterT \r -> do
  let
    Tuple d cases' = coerce $ for cases $
      mapper >>> case _ of
        WriterT f -> case f r of
          Tuple (Tuple pats a) d ->
            Tuple (coerce (removePats pats d) :: ADemands) (a d)
  Tuple (cases' <@> d) d

removePats :: Array ErlPattern -> MDemands -> MDemands
removePats pats (Multiplicative (Demands g m)) = Multiplicative $ Demands g $
  foldr (Map.delete) m (pats >>= names)
removePats _ (Multiplicative NullAndVoid) = Multiplicative NullAndVoid

names :: ErlPattern -> Array String
names S.Discard = mempty
names (S.MatchLiteral _) = mempty
names (S.BindVar name) = [name]
names (S.MatchBoth name pat) = [name] <> names pat
names (S.MatchMap pats) = foldMap (names <<< snd) pats
names (S.MatchTuple pats) = foldMap names pats
names (S.MatchList pats mpat) = foldMap names pats <> foldMap names mpat

spyDemands :: MDemands -> Array (Array GlobalErl)
spyDemands (Multiplicative (Demands ds _)) =
  Map.toUnfoldable ds <#> \(Tuple a b) -> [a, unsafeCoerce b]
spyDemands _ = []

knownByMatching :: ErlExpr -> ErlPattern -> MDemands
knownByMatching expr (S.MatchLiteral (S.Atom "true")) =
  knownFrom expr
knownByMatching _ _ = mempty

-- | One of the key pieces: we need to listen to `?IS_KNOWN_TAG` to determine
-- | what constructor applies to each branch. This is recorded as a `Demand`.
knownFrom :: ErlExpr -> MDemands
knownFrom (S.BinOp S.AndAlso e1 e2) = knownFrom e1 <> knownFrom e2
knownFrom (S.Macro "IS_KNOWN_TAG" (Just args))
  | [S.Literal (S.Atom tag), S.Literal (S.Integer arity), S.Var name acsrs] <- NEA.toArray args =
    Multiplicative $ Demands Map.empty $ Map.singleton name $ demandAccessors' acsrs $ DemandADT 1 (Just (Tuple tag arity)) Map.empty
knownFrom _ = mempty

-- | Constrain information that can leak through function scopes. We allow only
-- | global pure functions to be optimized through this (except if the function
-- | always throws an error, as indicated by `NullAndVoid`).
scoped :: forall a. W a -> W a
scoped = censor $ over Multiplicative case _ of
  NullAndVoid -> Demands Map.empty Map.empty
  Demands _ d -> Demands Map.empty d

mightNotRun :: forall a. W a -> W a
mightNotRun = censor $ over Multiplicative $ add one

-- | This is almost `StateT`. We pass demand information downwards mostly
-- | because of `knownFrom`, which is annoying, but maybe is the right choice
-- | anyways. (Can this be made one pass then?) Then the completed demand
-- | analysis is written out.
type W = WriterT MDemands ((->) MDemands)

withKnown :: forall a. MDemands -> W a -> W (Tuple MDemands a)
withKnown x (WriterT f) = WriterT \r -> case f (x <> r) of
  Tuple a y -> Tuple (Tuple y a) y

-- | This analyzes demand (via Reader+Writer) and uses that information to
-- | insert patterns to short-cut that demand.
optimizePatternDemand :: ErlExpr -> W ErlExpr
optimizePatternDemand = case _ of
  S.Assignments asgns ret -> WriterT \r -> do
    let Tuple asgns' ds = runWriterT (opdss asgns) r
    let Tuple ret' d = runWriterT (opd ret) r
    let d' = ds <> d
    -- NOTE: this shorthand only works if the names assigned in this block are
    -- distinct
    flip Tuple (removePats (map fst asgns) d') $ S.Assignments (lmap (choosePattern d') <$> asgns') ret'
  S.Fun name cases -> scoped do
    cases' <- alts cases \(Tuple (FunHead pats mg) e) -> do
      mg' <- opdg mg
      let k = foldMap knownFrom (coerce <$> mg)
      e' <- withKnown k (opd e)
      pure $ Tuple pats \d ds -> Tuple
        (FunHead (choosePattern d <$> pats) mg')
        (optimizeDemands ds e')
    pure $ S.Fun name cases'
  S.Case expr cases -> do
    expr' <- opd expr
    cases' <- alts cases \(CaseClause pat mg e) -> do
      mg' <- opdg mg
      let k = knownByMatching expr pat <> foldMap knownFrom (coerce <$> mg)
      e' <- withKnown k (opd e)
      pure $ Tuple [pat] \d ds -> CaseClause
        (choosePattern d pat)
        mg'
        (optimizeDemands ds e')
    pure $ S.Case expr' cases'
  S.If cases -> do
    cases' <- alts cases \(IfClause cond e) -> do
      cond' <- coerce opd cond
      let k = knownFrom (coerce cond)
      e' <- withKnown k (opd e)
      pure $ Tuple [] \_d ds -> IfClause cond' (optimizeDemands ds e')
    -- The first conditional always runs, so we want to include it in the analysis
    -- void $ censor justDemands $ opd $ (\(IfClause (Guard cond) _) -> cond) (NEA.head cases)
    pure $ S.If cases'
  e@(S.Var name acsrs) -> WriterT \r -> do
    let
      d = Multiplicative $ Demands Map.empty $ Map.singleton name $
        demandAccessors acsrs * fromMaybe one (demandOf name r)
    Tuple e d

  -- - Failures, where we discard demands
  s@(S.FunCall (Just (S.Literal (S.Atom "erlang"))) (S.Literal (S.Atom "error"))
    [S.Tupled [ S.Literal (S.Atom "fail"), S.Literal (S.String _i) ]]) ->
    WriterT \_ -> flip Tuple (Multiplicative NullAndVoid) s
  -- s@(S.FunCall (Just (S.Literal (S.Atom "erlang"))) (S.Literal (S.Atom "throw")) [_]) ->
  --   Tuple (Multiplicative NullAndVoid) s
  -- s@(S.FunCall (Just (S.Literal (S.Atom "erlang"))) (S.Literal (S.Atom "error")) [_]) ->
  --   Tuple (Multiplicative NullAndVoid) s
  -- s@(S.FunCall (Just (S.Literal (S.Atom "erlang"))) (S.Literal (S.Atom "exit")) [_]) ->
  --   Tuple (Multiplicative NullAndVoid) s

  s | Just global <- globalThunk s ->
    s <$ tell do
      Multiplicative $ Demands (Map.singleton global (Demand 1)) Map.empty

  e@(S.Literal _) -> pure e
  S.List es -> S.List <$> opds es
  S.ListCons es e -> S.ListCons <$> opds es <*> opd e
  S.Tupled es -> S.Tupled <$> opds es
  S.Map kvs -> S.Map <$> traverse (bitraverse opd opd) kvs
  S.MapUpdate e kvs -> S.MapUpdate <$> opd e <*> traverse (bitraverse opd opd) kvs
  S.Record kvs -> S.Record <$> coerce (opds (Compose kvs))
  S.RecordUpdate e kvs -> S.RecordUpdate <$> opd e <*> opdss kvs
  S.FunCall me e es -> S.FunCall <$> opds me <*> opd e <*> opds es
  S.FunName me e arity -> S.FunName <$> opds me <*> opd e <@> arity
  S.Macro name (Just args) | "MEMOIZE_AS" <- name ->
    pure $ S.Macro name $ Just $ (fst <<< flip runWriterT mempty <<< opd) <$> args
  S.Macro name margs -> S.Macro name <$> opdss margs
  S.BinOp op e1 e2 | shortCircuits op ->
    S.BinOp op <$> opd e1 <*> mightNotRun (opd e2)
  S.BinOp op e1 e2 -> S.BinOp op <$> opd e1 <*> opd e2
  S.UnaryOp op e -> S.UnaryOp op <$> opd e
  S.BinaryAppend e1 e2 -> S.BinaryAppend <$> opd e1 <*> opd e2

-- | Recognize a call to a global Erlang pure function (one with no arguments).
globalThunk :: ErlExpr -> Maybe GlobalErl
-- Unqualified references to functions in this module.
globalThunk (S.FunCall Nothing (S.Literal (S.Atom name)) []) = Just (GlobalErl { module: Nothing, name })
-- Qualified references to PureScript functions in other modules.
globalThunk (S.FunCall (Just (S.Literal (S.Atom mod))) (S.Literal (S.Atom name)) [])
  | Just _ <- String.stripSuffix (String.Pattern "@ps") mod =
  Just (GlobalErl { module: Just mod, name: name })
globalThunk _ = Nothing

-- | The information we keep around for rewrites: how to get accessors of
-- | variables/global calls. (Nested accessors may still rewrite to accessors,
-- | though some will short-cut.)
type VRW =
  { locals :: SemigroupMap String Rewrites
  , calls :: SemigroupMap GlobalErl Rewrites
  }
data Rewrites = Rewrites ErlExpr (Map Accessor Rewrites)
instance Semigroup Rewrites where
  append (Rewrites e1 rws1) (Rewrites e2 rws2) = Rewrites e3 $ Map.unionWith append rws1 rws2
    where
    e3 = case e1, e2 of
      S.Var _ acsrs1, S.Var _ acsrs2
        | Array.length acsrs1 <= Array.length acsrs2
        -> e1
      _, _ -> e2

getVar :: String -> Accessors -> VRW -> ErlExpr
getVar name acsrs { locals: SemigroupMap vrw } = case Map.lookup name vrw of
  Nothing -> S.Var name acsrs
  Just rw0 ->
    let
      find1 (Rewrites e m) acs =
        case Map.lookup acs m of
          Just r -> r
          Nothing -> Rewrites (access acs e) Map.empty
    in foldl find1 rw0 acsrs # \(Rewrites e _) -> e

bindVarFrom :: ErlExpr -> ErlPattern -> Rewrites
bindVarFrom e S.Discard = Rewrites e Map.empty
bindVarFrom e (S.MatchLiteral _) = Rewrites e Map.empty
bindVarFrom _ (S.BindVar name) = Rewrites (S.Var name self) Map.empty
bindVarFrom _ (S.MatchBoth name pat) = bindVarFrom (S.Var name self) pat
bindVarFrom e (S.MatchMap kvs) = Rewrites e $ Map.fromFoldable $
  kvs <#> \(Tuple k pat) ->
    Tuple (AcsKey k) (bindVarFrom (access (AcsKey k) e) pat)
bindVarFrom e (S.MatchTuple vs) = Rewrites e $ Map.fromFoldable $
  vs # mapWithIndex \idx pat ->
    Tuple (AcsElement (idx + 1)) (bindVarFrom (access (AcsElement (idx + 1)) e) pat)
bindVarFrom e (S.MatchList _ _) = Rewrites e Map.empty

bindVar :: ErlPattern -> VRW -> VRW
bindVar (S.MatchBoth name pat) = \vrw ->
  vrw <> do
    { calls: mempty, locals: SemigroupMap $ Map.singleton name $ bindVarFrom (S.Var name self) pat }
bindVar _ = identity

bindVarWithin :: ErlExpr -> ErlPattern -> VRW -> VRW
bindVarWithin (S.Var name acsrs) pat = \vrw ->
  vrw <> do
    { calls: mempty, locals: _ } $
      SemigroupMap $ Map.singleton name $
        snd $ foldr
          (\acsr (Tuple soFar r) -> Tuple (Array.dropEnd 1 soFar) $ Rewrites (getVar name soFar vrw) $ Map.singleton acsr r)
          (Tuple (Array.dropEnd 1 acsrs) (bindVarFrom (getVar name acsrs vrw) pat))
          acsrs
bindVarWithin s pat | Just global <- globalThunk s = \vrw ->
  vrw <> do
    let e = applyCall unit global (callErl [])
    { calls: _, locals: mempty } $
      SemigroupMap $ Map.singleton global $
        bindVarFrom e pat
bindVarWithin _ pat = bindVar pat

bindVars :: forall f. Foldable f => f ErlPattern -> VRW -> VRW
bindVars = unwrap <<< foldMap (Endo <<< bindVar)

opr :: ErlExpr -> VRW -> ErlExpr
opr e = optimizePatternRewrite e
oprs :: forall f. Traversable f => f ErlExpr -> VRW -> (f ErlExpr)
oprs es = traverse opr es
oprss :: forall f g. Traversable f => Traversable g => f (g ErlExpr) -> VRW -> (f (g ErlExpr))
oprss ess = traverse (traverse opr) ess
oprg :: Maybe Guard -> VRW -> (Maybe Guard)
oprg mg = traverse (coerce opr) mg

-- | This optimizes patterns by looking up names bound in binders.
optimizePatternRewrite :: ErlExpr -> VRW -> ErlExpr
optimizePatternRewrite = case _ of
  S.Var name acsrs -> getVar name acsrs
  S.Assignments asgns ret -> do
    let
      asgn1 vrw (Tuple pat e) =
        -- I think some of this is needless complexity that ended up not
        -- being necessary?
        case S.optimizePattern pat, opr e vrw of
          S.Discard, e' | S.guardExpr e' -> { value: Nothing, accum: vrw }
          S.BindVar v', S.Var v [] -> { value: Nothing, accum: bindVarWithin (S.Var v' []) (S.BindVar v) vrw }
          S.MatchBoth v' pat', e'@(S.Var v []) ->
            { value: Just (Tuple pat' e'), accum: bindVarWithin e pat' (bindVarWithin (S.Var v' []) (S.BindVar v) vrw) }
          pat', e' ->
            { value: Just (Tuple pat' e'), accum: bindVarWithin e pat' vrw }
    { value: asgns', accum: vrw } <- \vrw0 -> mapAccumL asgn1 vrw0 asgns
    -- if Array.length (Array.nub (names <<< fst =<< asgns)) == Array.length (names <<< fst =<< asgns) then pure unit else
    --   unsafeCrashWith "Repeat names"
    pure $ S.Assignments (Array.catMaybes asgns') (opr ret vrw)
  S.Fun name cases -> do
    cases' <- for cases \(Tuple (FunHead pats mg) e) -> bindVars pats >>> do
      mg' <- oprg mg
      e' <- opr e
      pure $ Tuple (FunHead pats mg') e'
    pure $ S.Fun name cases'
  S.Case expr cases -> do
    expr' <- optimizePatternRewrite expr
    cases' <- for cases \(CaseClause pat mg e) -> bindVar pat >>> do
      mg' <- oprg mg
      e' <- opr e
      pure $ CaseClause pat mg' e'
    pure $ S.Case expr' cases'
  S.If cases -> do
    cases' <- for cases \(IfClause cond e) -> do
      cond' <- coerce opr cond
      e' <- opr e
      pure $ IfClause cond' e'
    pure $ optimizeIf cases'
    -- pure $ S.If cases'

  s | Just global <- globalThunk s -> \{ calls: SemigroupMap calls } ->
    case Map.lookup global calls of
      Just (Rewrites s' _) -> s'
      Nothing -> s

  e@(S.Literal _) -> pure e
  S.List es -> S.List <$> oprs es
  S.ListCons es e -> S.ListCons <$> oprs es <*> opr e
  S.Tupled es -> S.Tupled <$> oprs es
  S.Map kvs -> S.Map <$> traverse (bitraverse opr opr) kvs
  S.MapUpdate e kvs -> S.MapUpdate <$> opr e <*> traverse (bitraverse opr opr) kvs
  S.Record kvs -> S.Record <$> coerce (oprs (Compose kvs))
  S.RecordUpdate e kvs -> S.RecordUpdate <$> opr e <*> oprss kvs
  S.FunCall me e es -> S.FunCall <$> oprs me <*> opr e <*> oprs es
  S.FunName me e arity -> S.FunName <$> oprs me <*> opr e <@> arity
  S.Macro name margs -> S.Macro name <$> oprss margs
  S.BinOp op e1 e2 -> S.BinOp op <$> opr e1 <*> opr e2
  S.UnaryOp op e -> S.UnaryOp op <$> opr e
  S.BinaryAppend e1 e2 -> S.BinaryAppend <$> opr e1 <*> opr e2

matchTag
  :: Guard
  -> Maybe
    { acsrs :: Array Accessor
    , arity :: Int
    , name :: String
    , tag :: String
    }
matchTag (S.Guard (S.Macro "IS_KNOWN_TAG" (Just args)))
  | [S.Literal (S.Atom tag), S.Literal (S.Integer arity), S.Var name acsrs] <- NEA.toArray args
  = Just { tag, arity, name, acsrs }
matchTag _ = Nothing

-- | Admittedly this is mostly vanity, but it produces nicer Erlang to turn
-- | the style of branching produced by backend-optimizer back into idiomatic
-- | Erlang case expressions.
optimizeIf :: NonEmptyArray IfClause -> ErlExpr
optimizeIf cases = case NEA.head cases of
  IfClause (S.Guard (S.Literal (S.Atom "true"))) body -> body
  IfClause cond _
    | Just { name, acsrs } <- matchTag cond
    , Just newCases <- caseOn name acsrs cases
    ->
      S.Case (S.Var name acsrs) $ NEA.appendArray newCases $
        case NEA.fromArray (NEA.drop (NEA.length newCases) cases) of
          Nothing -> []
          Just fallbacks -> pure $
            CaseClause S.Discard Nothing
              (optimizeIf fallbacks)
  _ -> S.If cases

caseOn :: String -> Array Accessor -> NonEmptyArray IfClause -> Maybe (NonEmptyArray CaseClause)
caseOn name acsrs cases = NEA.fromArray <=< sequence $ NEA.takeWhile isJust $
  cases <#> case _ of
    IfClause cond matching
      | Just { tag, arity, name: name', acsrs: acsrs' } <- matchTag cond
      , matchHere <- S.MatchTuple $ [S.MatchLiteral (S.Atom tag)] <> Array.replicate arity S.Discard
      , name == name', acsrs == acsrs'
      , Just (Tuple pat body) <- reassign name acsrs matching
      , Just pat' <- zipPat matchHere pat
      -> Just (CaseClause pat' Nothing body)
    _ -> Nothing


reassign :: String -> Array Accessor -> ErlExpr -> Maybe (Tuple ErlPattern ErlExpr)
reassign name acsrs (S.Assignments [] e) =
  reassign name acsrs e
reassign name acsrs (S.Assignments a1 (S.Assignments a2 e)) =
  reassign name acsrs (S.Assignments (a1 <> a2) e)
reassign name acsrs (S.Assignments asgns e) =
  let
    matches (Tuple _ (S.Var name' acsrs')) = name == name' && acsrs == acsrs'
    matches _ = false
  in case Array.partition matches asgns of
    { yes, no } | Just pat <- zipPats (map fst yes) ->
      Just $ Tuple pat case no of
        [] -> e
        _ -> S.Assignments no e
    _ -> Nothing
reassign _ _ e = Just (Tuple S.Discard e)

zipPats :: Array ErlPattern -> Maybe ErlPattern
zipPats pats = foldl (\mp p -> mp >>= zipPat p) (Just S.Discard) pats

zipPat :: ErlPattern -> ErlPattern -> Maybe ErlPattern
zipPat S.Discard p = Just p
zipPat p S.Discard = Just p
zipPat (S.BindVar v) p = Just $ S.MatchBoth v p
zipPat p (S.BindVar v) = Just $ S.MatchBoth v p
zipPat (S.MatchBoth v p1) p2 = S.MatchBoth v <$> zipPat p1 p2
zipPat p1 (S.MatchBoth v p2) = S.MatchBoth v <$> zipPat p1 p2
zipPat (S.MatchLiteral l1) (S.MatchLiteral l2) =
  S.MatchLiteral l1 <$ guard (l1 == l2)
zipPat (S.MatchTuple t1) (S.MatchTuple t2) | Array.length t1 == Array.length t2 =
  S.MatchTuple <$> sequence (Array.zipWith zipPat t1 t2)
zipPat _ _ = Nothing

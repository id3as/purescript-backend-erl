module PureScript.Backend.Erl.Convert.After where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Extend (duplicate)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Functor.Compose (Compose(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Endo (Endo(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Newtype (over, unwrap)
import Data.Semigroup.Last (Last(..))
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, for, mapAccumL, sum, traverse, traverse_)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Erl.Convert.Scoping (renameRoot)
import PureScript.Backend.Erl.Syntax (Accessor(..), Accessors, CaseClause(..), ErlDefinition(..), ErlExpr(..), ErlPattern, FunHead(..), Guard(..), IfClause(..), access, self)
import PureScript.Backend.Erl.Syntax as S
import Safe.Coerce (coerce)

data Demand
  = Unreachable
  | NoDemand
  | Demand Int
  | DemandADT Int (Maybe (Tuple String Int)) (Map Int Demand)
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

data Demands
  = Demands (Map String Demand)
  | NullAndVoid

type MDemands = Multiplicative Demands
type ADemands = Additive Demands

instance semiringDemands :: Semiring Demands where
  zero = NullAndVoid
  one = Demands Map.empty

  add (Demands d1) (Demands d2) = Demands $
    Map.union (lift2 add d1 d2) (NoDemand <$ (d1 <|> d2))
  add d@(Demands _) _ = d
  add _ d@(Demands _) = d
  add NullAndVoid NullAndVoid = NullAndVoid
  mul (Demands d1) (Demands d2) = Demands $
    Map.unionWith mul d1 d2
  mul _ _ = NullAndVoid

optimizePatternsDecl :: ErlDefinition -> ErlDefinition
optimizePatternsDecl (FunctionDefinition name args expr) =
  case optimizePatterns (S.Fun Nothing [ Tuple (FunHead args Nothing) expr ]) of
    S.Fun Nothing [ Tuple (FunHead args' Nothing) expr' ] ->
      FunctionDefinition name args' expr'
    _ -> unsafeCrashWith "Did not receive function of right shape in optimizePatternsDecl"

optimizePatterns :: ErlExpr -> ErlExpr
optimizePatterns =
  optimizePatternDemand
    >>> (\(Tuple _ e) -> optimizePatternRewrite e mempty)
    >>> renameRoot


opd :: ErlExpr -> Tuple (Multiplicative Demands) ErlExpr
opd e = optimizePatternDemand e
opds :: forall f. Traversable f => f ErlExpr -> Tuple (Multiplicative Demands) (f ErlExpr)
opds es = traverse opd es
opdss :: forall f g. Traversable f => Traversable g => f (g ErlExpr) -> Tuple (Multiplicative Demands) (f (g ErlExpr))
opdss ess = traverse (traverse opd) ess
opdg :: Maybe Guard -> Tuple (Multiplicative Demands) (Maybe Guard)
opdg mg = traverse (coerce opd) mg

choosePattern :: MDemands -> ErlPattern -> ErlPattern
choosePattern (Multiplicative (Demands m)) = case _ of
  S.BindVar v | Just d <- Map.lookup v m -> do
    S.MatchBoth v (makePattern v d)
  p -> p
choosePattern _ = identity

makePattern :: String -> Demand -> ErlPattern
makePattern _ (DemandRecord _ fields) | Map.isEmpty fields = S.Discard
makePattern v (DemandRecord _ fields) = S.MatchMap $
  Map.toUnfoldable fields # mapWithIndex \i (Tuple k demand) -> do
    let v' = v <> "@@" <> show i
    Tuple k (S.MatchBoth v' (makePattern v' demand))
makePattern _ (DemandADT _ (Just (Tuple _ 0)) _) = S.Discard
makePattern v (DemandADT _ (Just (Tuple tag arity)) fields) = S.MatchTuple $ [S.Discard] <> do
  Array.replicate arity unit # mapWithIndex \i _ -> case Map.lookup (i+2) fields of
    Nothing -> S.Discard
    Just demand -> do
      let v' = v <> "@@" <> show i
      S.MatchBoth v' (makePattern v' demand)
makePattern _ _ = S.Discard

demandAccessors :: Accessors -> Demand
demandAccessors = demandAccessors' <@> Demand 1

demandAccessors' :: Accessors -> Demand -> Demand
demandAccessors' =
  flip $ foldr $
    flip case _, _ of
      d, AcsElement idx -> DemandADT 0 Nothing $ Map.singleton idx d
      d, AcsKey key -> DemandRecord 0 $ Map.singleton key d

allDemand :: Demand -> Int
allDemand (Demand i) = i
allDemand (DemandADT i _ ds) = i + sum (allDemand <$> ds)
allDemand (DemandRecord i ds) = i + sum (allDemand <$> ds)
allDemand NoDemand = 0
allDemand Unreachable = 0

addedDemands :: Maybe MDemands -> MDemands -> MDemands -> Array (Tuple ErlPattern ErlExpr)
addedDemands helper (Multiplicative (Demands d1)) (Multiplicative (Demands d2)) =
  Array.mapMaybe (uncurry addBinding) $ Map.toUnfoldable $ Map.mapMaybeWithKey ((#)) $ lift2 addedDemand d1 d2
  where
  addedDemand _ d _ | allDemand d < 2 = Nothing
  addedDemand NoDemand (DemandADT d Nothing ds) k
    | Just (Multiplicative (Demands m)) <- helper
    , Just (DemandADT _ (Just r) _) <- Map.lookup k m =
      Just (DemandADT d (Just r) ds)
  addedDemand NoDemand d _ = Just d
  addedDemand _ _ _ = Nothing
  addBinding v demand = case makePattern v demand of
    S.Discard -> Nothing
    pat -> Just $ Tuple pat (S.Var v self)
addedDemands _ _ _ = []

optimizeDemands :: MDemands -> Maybe MDemands -> Tuple MDemands ErlExpr -> ErlExpr
optimizeDemands agg helper (Tuple this e) =
  Assignments (Array.reverse $ addedDemands helper (agg) (this)) e

alts ::
  forall f a.
    Traversable f =>
  f a ->
  (a -> Tuple MDemands (Tuple (Array ErlPattern) (MDemands -> MDemands -> a))) ->
  Tuple MDemands (f a)
alts cases mapper = do
  let
    Tuple d cases' = coerce $ for cases $
      mapper >>> \(Tuple d (Tuple pats a)) ->
        Tuple (coerce (removePats pats d) :: ADemands) (a d)
  Tuple d (cases' <@> d)

removePats :: Array ErlPattern -> MDemands -> MDemands
removePats pats (Multiplicative (Demands m)) = Multiplicative $ Demands $
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

spyDemands :: MDemands -> Array _
spyDemands (Multiplicative (Demands ds)) = Map.toUnfoldable ds
spyDemands _ = []

knownByMatching :: ErlExpr -> ErlPattern -> Tuple MDemands Unit
knownByMatching expr (S.MatchLiteral (S.Atom "true")) =
  knownFrom expr
knownByMatching _ _ = pure unit

knownFrom :: ErlExpr -> Tuple MDemands Unit
knownFrom (S.BinOp S.AndAlso e1 e2) = knownFrom e1 *> knownFrom e2
knownFrom (S.Macro "IS_KNOWN_TAG" (Just args))
  | [S.Literal (S.Atom tag), S.Literal (S.Integer arity), S.Var name acsrs] <- NEA.toArray args =
    Tuple (Multiplicative $ Demands $ Map.singleton name $ demandAccessors' acsrs $ DemandADT 1 (Just (Tuple tag arity)) Map.empty) unit
knownFrom _ = pure unit

scoped :: forall a. Tuple MDemands a -> Tuple MDemands a
scoped = lmap $ over Multiplicative case _ of
  NullAndVoid -> Demands Map.empty
  Demands m -> Demands m

optimizePatternDemand :: ErlExpr -> Tuple MDemands ErlExpr
optimizePatternDemand = case _ of
  S.Assignments asgns ret -> do
    let Tuple ds asgns' = opdss asgns
    let Tuple d ret' = opd ret
    let d' = ds <> d
    -- NOTE: this shorthand only works if the names assigned in this block are
    -- distinct
    Tuple (removePats (map fst asgns) d') $ S.Assignments (lmap (choosePattern d') <$> asgns') ret'
  S.Fun name cases -> scoped do
    cases' <- alts cases \(Tuple (FunHead pats mg) e) -> do
      mg' <- opdg mg
      traverse_ knownFrom (coerce <$> mg)
      e' <- duplicate (opd e)
      pure $ Tuple pats \d ds -> Tuple
        (FunHead (choosePattern d <$> pats) mg')
        (optimizeDemands ds (Just d) e')
    pure $ S.Fun name cases'
  S.Case expr cases -> do
    expr' <- opd expr
    cases' <- alts cases \(CaseClause pat mg e) -> do
      mg' <- opdg mg
      knownByMatching expr pat *> traverse_ knownFrom (coerce <$> mg)
      e' <- duplicate (opd e)
      pure $ Tuple [pat] \d ds -> CaseClause
        (choosePattern d pat)
        mg'
        (optimizeDemands ds (Just d) e')
    pure $ S.Case expr' cases'
  S.If cases -> do
    cases' <- alts cases \(IfClause cond e) -> do
      cond' <- opd cond
      knownFrom (coerce cond)
      e' <- duplicate (opd e)
      pure $ Tuple [] \d ds -> IfClause cond' (optimizeDemands ds (Just d) e')
    pure $ S.If cases'
  e@(S.Var name acsrs) -> do
    let d = Multiplicative $ Demands $ Map.singleton name $ demandAccessors acsrs
    Tuple d e

  -- A few special things to handle here:
  -- - Failures, where we discard demands
  s@(S.FunCall (Just (S.Literal (S.Atom "erlang"))) (S.Literal (S.Atom "throw")) [_]) ->
    Tuple (Multiplicative NullAndVoid) s
  s@(S.FunCall (Just (S.Literal (S.Atom "erlang"))) (S.Literal (S.Atom "error")) [_]) ->
    Tuple (Multiplicative NullAndVoid) s
  s@(S.FunCall (Just (S.Literal (S.Atom "erlang"))) (S.Literal (S.Atom "exit")) [_]) ->
    Tuple (Multiplicative NullAndVoid) s

  -- S.BinOp S.AndAlso e1 e2 ->
  --   S.BinOp S.AndAlso <$> opd e1 <* knownFrom e1 <*> opd e2


  e@(S.Literal _) -> pure e
  S.List es -> S.List <$> opds es
  S.ListCons es e -> S.ListCons <$> opds es <*> opd e
  S.Tupled es -> S.Tupled <$> opds es
  S.Map kvs -> S.Map <$> coerce (opds (Compose kvs))
  S.MapUpdate e kvs -> S.MapUpdate <$> opd e <*> opdss kvs
  S.FunCall me e es -> S.FunCall <$> opds me <*> opd e <*> opds es
  S.Macro name margs -> S.Macro name <$> opdss margs
  S.BinOp op e1 e2 -> S.BinOp op <$> opd e1 <*> opd e2
  S.UnaryOp op e -> S.UnaryOp op <$> opd e
  S.BinaryAppend e1 e2 -> S.BinaryAppend <$> opd e1 <*> opd e2
  -- binders ->
  --   collectDemand
  --   decideBinders
  --   applyBinders
  --   returnAggregateDemand


type VRW = SemigroupMap String (Last Rewrites)
data Rewrites = Rewrites ErlExpr (Map Accessor Rewrites)

getVar :: String -> Accessors -> VRW -> ErlExpr
getVar name acsrs (SemigroupMap vrw) = case Map.lookup name vrw of
  Nothing -> S.Var name acsrs
  Just (Last rw0) ->
    let
      find1 (Rewrites e m) acs =
        case Map.lookup acs m of
          Just r -> r
          Nothing -> Rewrites (access acs e) Map.empty
    in foldl find1 rw0 acsrs # \(Rewrites e _) -> e

mta = Map.toUnfoldable >>> Array.mapMaybe Just

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
    SemigroupMap $ Map.singleton name $ Last $ bindVarFrom (S.Var name self) pat
bindVar _ = identity

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

optimizePatternRewrite :: ErlExpr -> VRW -> ErlExpr
optimizePatternRewrite = case _ of
  S.Var name acsrs -> getVar name acsrs
  S.Assignments asgns ret -> do
    let
      asgn1 vrw (Tuple pat e) = do
        let
          pat' = case pat, e of
            S.MatchBoth _ _, _ -> pat
            _, S.Var v [] -> S.MatchBoth v pat
            _, _ -> pat
        { value: Tuple pat (opr e vrw), accum: bindVar pat' vrw }
    { value: asgns', accum: vrw } <- \vrw0 -> mapAccumL asgn1 vrw0 asgns
    pure $ S.Assignments asgns' (opr ret vrw)
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
      cond' <- opr cond
      e' <- opr e
      pure $ IfClause cond' e'
    pure $ S.If cases'

  e@(S.Literal _) -> pure e
  S.List es -> S.List <$> oprs es
  S.ListCons es e -> S.ListCons <$> oprs es <*> opr e
  S.Tupled es -> S.Tupled <$> oprs es
  S.Map kvs -> S.Map <$> coerce (oprs (Compose kvs))
  S.MapUpdate e kvs -> S.MapUpdate <$> opr e <*> oprss kvs
  S.FunCall me e es -> S.FunCall <$> oprs me <*> opr e <*> oprs es
  S.Macro name margs -> S.Macro name <$> oprss margs
  S.BinOp op e1 e2 -> S.BinOp op <$> opr e1 <*> opr e2
  S.UnaryOp op e -> S.UnaryOp op <$> opr e
  S.BinaryAppend e1 e2 -> S.BinaryAppend <$> opr e1 <*> opr e2

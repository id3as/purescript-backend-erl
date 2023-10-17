module PureScript.Backend.Erl.Convert.Scoping where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.State (State, evalState, gets, modify_, state)
import Data.Array (all)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Profunctor (dimap)
import Data.Semigroup.Last (Last(..))
import Data.String.CodeUnits as CU
import Data.Traversable (class Traversable, foldr, for, traverse)
import Data.Tuple (Tuple(..), snd, uncurry)
import PureScript.Backend.Erl.Syntax (CaseClause(..), ErlExpr, ErlPattern, FunHead(..), Guard(..), IfClause(..), self)
import PureScript.Backend.Erl.Syntax as S
import Safe.Coerce (coerce)

type Name = String
type Rename = String

type Renamings =
  Map Name Rename

type Found =
  { duplicates :: Map String Int
  , usages :: Map Rename Int
  }

type Renaming = ReaderT Renamings (State Found)

scope :: forall a. Renaming a -> Renaming a
scope inner = do
  s <- gets _.duplicates
  inner <* modify_ _ { duplicates = s }

use :: Rename -> Map Rename Int -> Map Rename Int
use renamed =
  Map.alter (Just <<< maybe 1 (add 1)) renamed

reference :: Name -> Renaming Rename
reference name = do
  renamed <- asks $ fromMaybe name <<< Map.lookup name
  modify_ \r -> r { usages = use renamed r.usages }
  pure renamed

isUnused :: Rename -> Renaming Rename
isUnused rename = gets $ _.usages
  >>> Map.lookup rename
  >>> maybe "_" (const rename)

ensure :: Name -> String
ensure n = case CU.takeWhile (_ /= '@') n of
  "" -> "V"
  -- For things like `_@dollar__unused`
  "_" -> "V"
  r -> r

choose :: Name -> Renaming Rename
choose name = state \found ->
  let i = fromMaybe 0 (Map.lookup name found.duplicates)
  in Tuple (newName name i) found { duplicates = Map.insert name (i+1) found.duplicates }

chooseMany :: forall f. Traversable f => f Name -> Renaming (f Rename)
chooseMany = traverse choose

choosePat :: ErlPattern -> Renaming ErlPattern
choosePat = overNames choose

overNames :: forall f. Applicative f => (Name -> f Name) -> ErlPattern -> f ErlPattern
overNames f = case _ of
  S.Discard -> pure S.Discard
  S.BindVar name -> f name <#> \rename ->
    if rename == "_" then S.Discard else S.BindVar rename
  p@(S.MatchLiteral _) -> pure p
  S.MatchBoth name pat -> S.MatchBoth <$> f name <*> overNames f pat
  S.MatchMap pats -> S.MatchMap <$> traverse (traverse (overNames f)) pats
  S.MatchTuple pats -> S.MatchTuple <$> traverse (overNames f) pats
  S.MatchList pats mpat -> S.MatchList
    <$> traverse (overNames f) pats
    <*> traverse (overNames f) mpat

newName :: String -> Int -> Rename
newName i 0 = i
newName "_" _ = "_"
newName i lvl = i <> "@" <> show lvl

asdf :: forall f. Traversable f => f ErlPattern -> Compose Renaming (Tuple (Map String (Last String))) (f ErlPattern)
asdf = traverse $ overNames \name -> Compose do
  choose (ensure name) <#> \rename ->
    Tuple (Map.singleton name (Last rename)) rename
  -- Compose $ choose (ensure name) <#> \rename ->

binding ::
  forall f b.
    Traversable f =>
  f ErlPattern -> Renaming b ->
  Renaming (Tuple (f ErlPattern) b)
binding names inner = do
  Tuple mapping renames <- unwrap $ asdf names
  let renaming = local $ Map.union $ unwrap <$> mapping
  flip Tuple <$> renaming inner <*> traverse (overNames isUnused) renames

scopedBinding ::
  forall f b.
    Traversable f =>
  f ErlPattern -> Renaming b ->
  Renaming (Tuple (f ErlPattern) b)
scopedBinding names inner = scope (binding names inner)

scopedBindingName ::
  forall b.
  String -> Renaming b ->
  Renaming (Tuple String b)
scopedBindingName name inner = scope do
  newName <- choose (ensure name)
  let renaming = local $ Map.insert name newName
  flip Tuple <$> renaming inner <*> isUnused newName


opr :: ErlExpr -> Renaming ErlExpr
opr e = renameTree e
oprs :: forall f. Traversable f => f ErlExpr -> Renaming (f ErlExpr)
oprs es = traverse opr es
oprss :: forall f g. Traversable f => Traversable g => f (g ErlExpr) -> Renaming (f (g ErlExpr))
oprss ess = traverse (traverse opr) ess
oprg :: Maybe Guard -> Renaming (Maybe Guard)
oprg mg = traverse (dimap coerce (map coerce) opr) mg

floating :: ErlExpr -> Tuple (ErlExpr -> ErlExpr) ErlExpr
floating (S.Assignments asgns e) = Tuple (S.Assignments asgns) e
floating e = Tuple identity e

renameTree :: ErlExpr -> Renaming ErlExpr
renameTree = case _ of
  S.Var name acsrs -> S.Var <$> (reference name) <@> acsrs
  S.Assignments asgns ret -> do
    let
      asgn1 (Tuple pat e) more = do
        lift2 Tuple (opr e) (binding (Identity pat) more) <#> \(Tuple e' (Tuple (Identity pat') (Tuple asgns' final'))) -> do
          Tuple (Array.cons (Tuple pat' e') asgns') final'
    uncurry S.Assignments <$> foldr asgn1 (Tuple [] <$> opr ret) asgns
  S.Fun (Just name) cases -> do
    uncurry (S.Fun <<< Just) <$> scopedBindingName name do
      for cases \(Tuple (FunHead pats mg) e) -> do
        (\(Tuple a (Tuple b c)) -> Tuple (FunHead a b) c) <$> scopedBinding pats do
          Tuple <$> oprg mg <*> opr e
  S.Fun Nothing cases -> do
    map (S.Fun Nothing) $
      for cases \(Tuple (FunHead pats mg) e) -> do
        (\(Tuple a (Tuple b c)) -> Tuple (FunHead a b) c) <$> scopedBinding pats do
          Tuple <$> oprg mg <*> opr e
  S.Case expr cases -> do
    Tuple floated expr' <- floating <$> opr expr
    cases' <- for cases \(CaseClause pat mg e) ->
      binding (Identity pat) do
        mg' <- oprg mg
        e' <- opr e
        pure $ Tuple mg' e'
    pure $ floated $ S.Case expr' $ cases' <#> \(Tuple (Identity pat') (Tuple mg' e')) ->
      CaseClause pat' mg' e'
  S.If cases ->
    S.If <$> for cases \(IfClause cond e) ->
      IfClause <$> opr cond <*> opr e

  e@(S.Literal _) -> pure e
  S.List es -> S.List <$> oprs es
  S.ListCons es e -> S.ListCons <$> oprs es <*> opr e
  S.Tupled es -> S.Tupled <$> oprs es
  S.Map kvs -> coerce S.Map <$> (oprs (Compose kvs))
  S.MapUpdate e kvs -> S.MapUpdate <$> opr e <*> oprss kvs
  S.FunCall me e es -> S.FunCall <$> oprs me <*> opr e <*> oprs es
  S.Macro name Nothing -> pure (S.Macro name Nothing)
  -- Macros that use their arguments multiple times are allergic to case
  -- statements that bind any variables, so we hoist arguments if needed
  S.Macro name (Just args) -> do
    Tuple bindings args' <- NEA.unzip <$> for args \arg -> do
      if trivialExpr arg then pure (Tuple Nothing arg) else do
        argName <- choose "V"
        arg' <- renameTree arg
        pure $ Tuple (Just (Tuple (S.BindVar argName) arg')) (S.Var argName self)
    S.Assignments (NEA.catMaybes bindings) <<< S.Macro name <<< Just <$> oprs args'
  S.BinOp op e1 e2 -> S.BinOp op <$> opr e1 <*> opr e2
  S.UnaryOp op e -> S.UnaryOp op <$> opr e
  S.BinaryAppend e1 e2 -> S.BinaryAppend <$> opr e1 <*> opr e2

trivialExpr :: ErlExpr -> Boolean
trivialExpr (S.Var _ _) = true
trivialExpr (S.Literal _) = true
trivialExpr (S.List items) = all trivialExpr items
trivialExpr (S.ListCons items tail) = all trivialExpr items && trivialExpr tail
trivialExpr (S.Map items) = all (trivialExpr <<< snd) items
trivialExpr (S.Tupled items) = all trivialExpr items
trivialExpr _ = false

renameRoot :: ErlExpr -> ErlExpr
renameRoot e = evalState (runReaderT (renameTree e) Map.empty)
  { duplicates: Map.empty, usages: Map.empty }

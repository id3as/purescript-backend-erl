module PureScript.Backend.Erl.Convert.Before where

import Prelude

import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.State (State, evalState, get, gets, modify_, put, state)
import Data.Array.NonEmpty as NEA
import Data.Identity (Identity(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (dimap)
import Data.Set as Set
import Data.Traversable (class Foldable, class Traversable, foldr, for, maximum, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Optimizer.CoreFn (Ident(..))
import PureScript.Backend.Optimizer.Semantics (NeutralExpr)
import PureScript.Backend.Optimizer.Syntax (BackendSyntax(..), Level(..))

type Name = Tuple (Maybe Ident) Level
type Rename = Name

type Renamings =
  Map Name Rename

type Found =
  { duplicates :: Map Ident Int
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
  >>> maybe (Tuple (fst rename) (Level (-1))) (const rename)

-- Anonymous variables need to be treated the same as variables named `v`
-- for the purpose of this analysis
ensure :: Name -> Ident
ensure = fst >>> fromMaybe (Ident "v")

choose :: Ident -> Renaming Rename
choose name = state \found ->
  let i = fromMaybe 0 (Map.lookup name found.duplicates)
  in Tuple (newName name i) found { duplicates = Map.insert name (i+1) found.duplicates }

chooseMany :: forall f. Foldable f => f Ident -> Renaming Level
chooseMany names = state \found ->
  let
    set = Set.toMap $ Set.fromFoldable names
    dups = found.duplicates <* set
    lvl = fromMaybe 0 (maximum dups)
    newDups = Map.union (lvl <$ set) found.duplicates
  in
    Tuple (Level lvl) found { duplicates = newDups }

newName :: Ident -> Int -> Rename
newName i lvl =
  Tuple (Just i) (Level lvl)

binding ::
  forall f b.
    Traversable f =>
  f Name -> Renaming b ->
  Renaming (Tuple (f Rename) b)
binding names inner = do
  mapping <- for names \name -> do
    Tuple name <$> choose (ensure name)
  let renaming = local $ Map.union $ Map.fromFoldable mapping
  let renames = map snd mapping
  flip Tuple <$> renaming inner <*> traverse isUnused renames

bindRec ::
  forall f b.
    Traversable f =>
  f Ident ->
  Level ->
  Renaming b ->
  Renaming (Tuple Level b)
bindRec idents lvl inner = do
  lvl' <- chooseMany idents
  let
    renaming = local $ Map.union $ Map.fromFoldable $ idents <#> \ident ->
      Tuple (Tuple (Just ident) lvl) (Tuple (Just ident) lvl')
  Tuple lvl' <$> renaming inner

scopedBinding ::
  forall f b.
    Traversable f =>
  f Name -> Renaming b ->
  Renaming (Tuple (f Rename) b)
scopedBinding names inner = scope (binding names inner)

renameTree :: NeutralExpr -> Renaming NeutralExpr
renameTree =  dimap unwrap (map wrap) case _ of
  Local i l -> uncurry Local <$> reference (Tuple i l)
  Abs names e -> uncurry Abs <$> scopedBinding names (renameTree e)
  UncurriedAbs names e -> uncurry UncurriedAbs <$> scopedBinding names (renameTree e)
  UncurriedEffectAbs names e -> uncurry UncurriedEffectAbs <$> scopedBinding names (renameTree e)
  LetRec l identsAndValues e -> do
    let idents = fst <$> identsAndValues
    Tuple l' (Tuple values e') <- bindRec idents l do
      Tuple <$> traverse (renameTree <<< snd) identsAndValues <*> renameTree e
    let identsAndValues' = NEA.zip idents values
    pure $ LetRec l' identsAndValues' e'
  Let i l v e -> ado
    v' <- renameTree v
    Tuple (Identity (Tuple i' l')) e' <-
      binding (Identity (Tuple i l)) (renameTree e)
    in Let i' l' v' e'
  EffectBind i l v e -> ado
    v' <- renameTree v
    Tuple (Identity (Tuple i' l')) e' <-
      binding (Identity (Tuple i l)) (renameTree e)
    in EffectBind i' l' v' e'
  s -> traverse renameTree s

renameRoot :: NeutralExpr -> NeutralExpr
renameRoot e = evalState (runReaderT (renameTree e) Map.empty)
  { duplicates: Map.empty, usages: Map.empty }

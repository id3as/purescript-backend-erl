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
import Data.Traversable (class Traversable, foldr, for, traverse)
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
  { duplicates :: Map Name Int
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
ensure :: Name -> Name
ensure (Tuple Nothing lvl) = Tuple (Just (Ident "v")) lvl
ensure i = i

choose :: Name -> Renaming Rename
choose = ensure >>> \name -> state \found ->
  case Map.lookup name found.duplicates of
    Nothing -> Tuple name found { duplicates = Map.insert name 1 found.duplicates }
    Just i -> Tuple (newName name i) found { duplicates = Map.insert name (i+1) found.duplicates }

newName :: Name -> Int -> Rename
newName (Tuple Nothing lvl) i =
  Tuple (Just (Ident ("v@" <> show i))) lvl
newName (Tuple (Just (Ident s)) lvl) i =
  Tuple (Just (Ident (s <> "@" <> show i))) lvl

binding ::
  forall f b.
    Traversable f =>
  f Name -> Renaming b ->
  Renaming (Tuple (f Rename) b)
binding names inner = do
  renamings <- for names \name -> do
    Tuple name <$> choose name
  let renaming = local $ Map.union $ Map.fromFoldable renamings
  let renames = map snd renamings
  flip Tuple <$> renaming inner <*> traverse isUnused renames

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
    let
      handle (Tuple ident value) more = do
        Tuple (Identity (Tuple identM l')) (Tuple value' (Tuple more' e')) <-
          binding (Identity (Tuple (Just ident) l))
            (Tuple <$> renameTree value <*> more)
        let
          ident' = case identM of
            Nothing -> unsafeCrashWith "Lost identifier"
            _ | l' /= l -> unsafeCrashWith "Level changed"
            Just i -> i
        pure $ Tuple ([Tuple ident' value'] <> more') e'
    Tuple identsAndValuesM e' <- foldr handle (Tuple [] <$> renameTree e) identsAndValues
    let
      identsAndValues' = case NEA.fromArray identsAndValuesM of
        Nothing -> unsafeCrashWith "Empty bindings"
        Just x -> x
    pure $ LetRec l identsAndValues' e'
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

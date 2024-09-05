module PureScript.Backend.Erl.Convert.Scoping where

import Prelude

import Control.Monad.Reader (ReaderT(..), asks, local, runReaderT)
import Control.Monad.State (State, StateT(..), evalState, gets, modify_, state)
import Data.Array (all)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Bitraversable (bitraverse)
import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity(..))
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (un, unwrap)
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

-- | Revert scope when leaving function scopes.
scope :: forall a. Renaming a -> Renaming a
scope inner = do
  s <- gets _.duplicates
  -- Do not reset usages because they may refer to outer scopes still
  inner <* modify_ _ { duplicates = s }

-- | Mark a variable as used.
use :: Rename -> Map Rename Int -> Map Rename Int
use = Map.alter (Just <<< maybe 1 (add 1))

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
  -- For things like `_@dollar__unused`, which do in fact get used as
  -- intermediate variables in currying/other function conversions,
  -- and we use `"_"` as a sentinel value for `Discard`.
  "_" -> "V"
  r -> r

-- | Choose a fresh name for a variable
choose :: Name -> Renaming Rename
choose name = state \found ->
  let i = fromMaybe 0 (Map.lookup name found.duplicates)
      rename = chooseNewName name i
  in Tuple rename
    { duplicates: Map.insert name (i+1) found.duplicates
    -- Usage does not get reset when pushing scopes horizontally (in scope)
    -- so we reset the new binding as we push it down vertically
    , usages: Map.delete rename found.usages
    }

chooseNewName :: String -> Int -> Rename
chooseNewName i 0 = i
chooseNewName "_" _ = "_"
chooseNewName i lvl = i <> "@" <> show lvl

-- | Traverse names in a pattern.
overNames :: forall f. Applicative f => (Name -> f Name) -> ErlPattern -> f ErlPattern
overNames f = case _ of
  S.Discard -> pure S.Discard
  S.BindVar name -> f name <#> \rename ->
    if rename == "_" then S.Discard else S.BindVar rename
  p@(S.MatchLiteral _) -> pure p
  S.MatchBoth name pat -> ado
    rename <- f name
    pat' <- overNames f pat
    in if rename == "_" then pat' else S.MatchBoth rename pat'
  S.MatchMap pats -> S.MatchMap <$> traverse (traverse (overNames f)) pats
  S.MatchTuple pats -> S.MatchTuple <$> traverse (overNames f) pats
  S.MatchList pats mpat -> S.MatchList
    <$> traverse (overNames f) pats
    <*> traverse (overNames f) mpat

-- | Rename several patterns at once.
renameMany :: forall f. Traversable f => f ErlPattern ->
  Compose Renaming (Tuple (SemigroupMap String (Last String))) (f ErlPattern)
renameMany = traverse $ overNames \name -> Compose do
  choose (ensure name) <#> \rename ->
    Tuple (SemigroupMap $ Map.singleton name $ Last rename) rename

-- | Bind some names in scope while renaming `inner` that linger around
-- | afterwards due to the scoping rules of Erlang.
binding ::
  forall f b.
    Traversable f =>
  f ErlPattern -> Renaming b ->
  Renaming (Tuple (f ErlPattern) b)
binding names inner = do
  Tuple (SemigroupMap mapping) renames <- un Compose $ renameMany names
  let renaming = local $ Map.union $ unwrap <$> mapping
  flip Tuple <$> renaming inner <*> traverse (overNames isUnused) renames

-- | Bind some names in scope while renaming `inner` that do NOT linger around
-- | afterwards, explicitly for function scopes.
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

alias :: forall b. Name -> Name -> Renaming b -> Renaming b
alias new old inner = do
  commonRename <- asks $ fromMaybe old <<< Map.lookup old
  local (Map.insert new commonRename) inner

opr :: ErlExpr -> Renaming ErlExpr
opr e = renameTree e
oprs :: forall f. Traversable f => f ErlExpr -> Renaming (f ErlExpr)
oprs es = traverse opr es
oprss :: forall f g. Traversable f => Traversable g => f (g ErlExpr) -> Renaming (f (g ErlExpr))
oprss ess = traverse (traverse opr) ess
oprg :: Maybe Guard -> Renaming (Maybe Guard)
oprg mg = traverse (dimap coerce (map coerce) opr) mg

-- | Float any assignments from the expression, returning them as a function
-- | to apply the floated variables again.
floating :: ErlExpr -> Tuple (ErlExpr -> ErlExpr) ErlExpr
floating (S.Assignments asgns1 (S.Assignments asgns2 e)) = floating (S.Assignments (asgns1 <> asgns2) e)
floating (S.Assignments [] e) = Tuple identity e
floating (S.Assignments asgns e) = Tuple (S.Assignments asgns) e
floating e = Tuple identity e

renameTree :: ErlExpr -> Renaming ErlExpr
renameTree = case _ of
  S.Var name acsrs -> S.Var <$> (reference name) <@> acsrs
  S.Assignments asgns ret -> do
    let
      asgn1 (Tuple (S.BindVar new) (S.Var old [])) more = do
        alias new old more
      asgn1 (Tuple pat e) more = ado
        e' <- opr e
        Tuple (Identity pat') (Tuple asgns' final) <- binding (Identity pat) more
        in Tuple (Array.cons (Tuple pat' e') asgns') final
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
      IfClause <$> coerce opr cond <*> opr e

  e@(S.Literal _) -> pure e
  S.List es -> S.List <$> oprs es
  S.ListCons es e -> S.ListCons <$> oprs es <*> opr e
  S.Tupled es -> S.Tupled <$> oprs es
  S.Map kvs -> S.Map <$> traverse (bitraverse opr opr) kvs
  S.MapUpdate e kvs -> S.MapUpdate <$> opr e <*> traverse (bitraverse opr opr) kvs
  S.Record kvs -> coerce S.Record <$> (oprs (Compose kvs))
  S.RecordUpdate e kvs -> S.RecordUpdate <$> opr e <*> oprss kvs

  S.FunCall Nothing fn args ->
    case floating fn of
      -- Beta-reduce, since erlc sometimes struggles with it
      Tuple floated (S.Fun Nothing [Tuple (FunHead pats Nothing) body])
        | Array.length pats == Array.length args ->
          renameTree $ floated <<< S.Assignments (Array.zip pats args) $ body
      -- If we see a function name like (fun x/2)(u, v)
      Tuple floated (S.FunName me e arity)
        | Array.length args == arity ->
          renameTree $ floated $ S.FunCall me e args
      -- Push function calls into cases
      -- (Should eliminate some unnecessary functions and have no runtime cost,
      -- although it may result in code duplication if the arguments are large)
      -- TODO: handle variables properly
      Tuple floated (S.Case expr cases) | Array.all noFreeVars args ->
          renameTree $ floated $ S.Case expr $ cases <#>
            \(CaseClause pat guard result) ->
              CaseClause pat guard (S.FunCall Nothing result args)
      Tuple floated (S.If cases) | Array.all noFreeVars args ->
          renameTree $ floated $ S.If $ cases <#>
            \(IfClause guard result) ->
              IfClause guard (S.FunCall Nothing result args)
      _ ->
        S.FunCall Nothing <$> opr fn <*> oprs args
  -- Otherwise do the usual thing
  S.FunCall me e es -> S.FunCall <$> oprs me <*> opr e <*> oprs es

  S.FunName me e arity -> S.FunName <$> oprs me <*> opr e <@> arity

  -- S.Macro name margs -> S.Macro name <$> traverse (traverse renameTree) margs
  S.Macro name Nothing -> pure (S.Macro name Nothing)
  -- Macros that use their arguments multiple times are allergic to case
  -- statements that bind any variables, so we hoist arguments if needed
  S.Macro name (Just args) -> do
    Tuple bindings args' <- NEA.unzip <$> for args \arg -> do
      if trivialExpr arg then Tuple Nothing <$> renameTree arg else do
        arg' <- renameTree arg
        argName <- choose "V"
        pure $ Tuple (Just (Tuple (S.BindVar argName) arg')) (S.Var argName self)
    pure $ S.Assignments (NEA.catMaybes bindings) $ S.Macro name $ Just args'

  S.BinOp op e1 e2 -> S.BinOp op <$> opr e1 <*> opr e2
  S.UnaryOp op e -> S.UnaryOp op <$> opr e
  S.BinaryAppend e1 e2 -> S.BinaryAppend <$> opr e1 <*> opr e2

-- | Trivial expressions do not need to be duplicated for macros.
trivialExpr :: ErlExpr -> Boolean
trivialExpr (S.Var _ _) = true
trivialExpr (S.Literal _) = true
trivialExpr (S.List items) = all trivialExpr items
trivialExpr (S.ListCons items tail) = all trivialExpr items && trivialExpr tail
trivialExpr (S.Map items) = all (trivialExpr <<< snd) items
trivialExpr (S.Tupled items) = all trivialExpr items
trivialExpr _ = false

noFreeVars :: ErlExpr -> Boolean
noFreeVars = coerce <<< S.visit case _ of
  S.Var _ _ -> Conj false
  _ -> mempty

renameRoot :: ErlExpr -> ErlExpr
renameRoot e = evalState (runReaderT (renameTree e) Map.empty)
  { duplicates: Map.empty, usages: Map.empty }

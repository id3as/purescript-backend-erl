module PureScript.Backend.Erl.Convert.Foreign where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), fromMaybe', maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import PureScript.Backend.Erl.Convert.Common (toErlVarExpr)
import PureScript.Backend.Erl.Syntax (ErlExpr, FunHead(..))
import PureScript.Backend.Erl.Syntax as S
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (NeutralExpr(..))
import PureScript.Backend.Optimizer.Syntax (BackendSyntax(..))

helper :: String -> String -> Int -> NeutralExpr -> Maybe (Array NeutralExpr)
helper moduleName ident = case _, _ of
  0, NeutralExpr (Var (Qualified (Just (ModuleName mn)) (Ident id)))
    | mn == moduleName, ident == id ->
      Just []
  arity, NeutralExpr (App (NeutralExpr (Var (Qualified (Just (ModuleName mn)) (Ident id)))) args)
    | mn == moduleName, ident == id ->
      if NEA.length args >= arity
        then Just (NEA.toArray args)
        else Nothing
  _, _ -> Nothing

tryMany :: forall a b. Array (a -> Maybe b) -> a -> Maybe b
tryMany options input = options # Array.findMap \fn -> fn input

tryMany' :: forall i a b. Array (Tuple i (a -> Maybe b)) -> a -> Maybe (Tuple i b)
tryMany' options input = options # Array.findMap \(Tuple i fn) -> Tuple i <$> fn input

codegenForeign :: (NeutralExpr -> ErlExpr) -> NeutralExpr -> Maybe ErlExpr
codegenForeign codegenExpr s = case unwrap s of
  Var _ -> codegenForeign' codegenExpr s
  App (NeutralExpr (Var _)) _ -> codegenForeign' codegenExpr s
  _ -> Nothing

codegenForeign' :: (NeutralExpr -> ErlExpr) -> NeutralExpr -> Maybe ErlExpr
codegenForeign' codegenExpr s = case unwrap s of
  _ | Just result <- codegenList codegenExpr s ->
    Just result

  _ | Just [NeutralExpr (Lit (LitString value))] <- helper "Erl.Atom" "atom" 1 s ->
    Just (S.Literal (S.Atom value))

  _ | Just [] <- helper "Data.Unit" "unit" 0 s ->
    Just (S.Literal (S.Atom "unit"))

  _ | Just args <- recognizeTuples s ->
    Just (S.Tupled (codegenExpr <$> args))

  _ | Just (Tuple arity [ NeutralExpr (Abs a e) ]) <- recognizeUncurry1s s
    , NEA.length a >= arity ->
      Just $ S.Fun Nothing $ Array.singleton $ Tuple (FunHead [ S.Tupled (toErlVarExpr <$> NEA.take arity a) ] Nothing) $
        codegenExpr (maybe e (\a' -> NeutralExpr (Abs a' e)) (NEA.fromArray $ NEA.drop arity a))

  _ -> Nothing

-- `tupleN` with N arguments applied
recognizeTuples :: NeutralExpr -> Maybe (Array NeutralExpr)
recognizeTuples = tryMany $ [1,2,3,4,5,6,7,8,9,10] <#>
  \i -> helper "Erl.Data.Tuple" ("tuple" <> show i) i

-- `uncurryN` with 1 argument applied
recognizeUncurry1s :: NeutralExpr -> Maybe (Tuple Int (Array NeutralExpr))
recognizeUncurry1s = tryMany' $ [1,2,3,4,5,6,7,8,9,10] <#>
  \i -> Tuple i $ helper "Erl.Data.Tuple" ("uncurry" <> show i) 1

codegenList :: (NeutralExpr -> ErlExpr) -> NeutralExpr -> Maybe ErlExpr
codegenList codegenExpr = gather [] <@> finish where
  typesMod = "Erl.Data.List.Types"
  listPrim = helper typesMod

  finish = { lit: finishLit, cons: finishCons }
  finishLit acc =
    Just (S.List (codegenExpr <$> acc))
  finishCons [] _ =
    Nothing
  finishCons acc s' =
    Just (finishCons' acc s')
  finishCons' [] s' = codegenExpr s'
  finishCons' acc s' =
    S.ListCons (codegenExpr <$> acc) (codegenExpr s')

  gather acc s end = case unit of
    _ | Just [head, tail] <- listPrim "cons" 2 s ->
      gather (acc <> [head]) tail end
    _ | Just [l, r] <- listPrim "appendImpl" 2 s ->
      gather acc l
        { lit: \acc' -> gather acc' r end
        , cons: \acc' l' ->
            Just (S.BinOp S.ListConcat (finishCons' acc' l') $ fromMaybe' (\_ -> codegenExpr r) (gather [] r finish))
        }
    -- TODO: use NeutStop to choose a different normal form for this Array ~> List?
    _ | Just [cons, nil, ls] <- helper "Data.Foldable" "foldrArray" 3 s
      , Just [] <- listPrim "cons" 0 cons
      , Just [] <- listPrim "nil" 0 nil
      , Lit (LitArray acc') <- unwrap ls ->
      end.lit (acc <> acc')
    _ | Just [] <- listPrim "nil" 0 s ->
      end.lit acc
    _ ->
      end.cons acc s

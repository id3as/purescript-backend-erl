module PureScript.Backend.Erl.Foreign ( erlForeignSemantics ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Lazy (force)
import Data.Lazy as Lazy
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Debug (spy, spyWith)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (BackendSemantics(..), EvalRef(..), ExternSpine(..), SemConditional(..), evalApp, evalPrimOp, liftInt, makeLet)
import PureScript.Backend.Optimizer.Semantics.Foreign (ForeignEval, ForeignSemantics, qualified)
import PureScript.Backend.Optimizer.Syntax (BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorOrd(..))

erlForeignSemantics :: Map (Qualified Ident) ForeignEval
erlForeignSemantics = Map.fromFoldable
  [ data_array_indexImpl
  , erl_data_list_types_appendImpl
  ]

helper :: String -> String -> Int -> BackendSemantics -> Maybe (Array BackendSemantics)
helper = helper' true

helper' :: Boolean -> String -> String -> Int -> BackendSemantics -> Maybe (Array BackendSemantics)
helper' shouldForce moduleName ident = case _, _ of
  0, NeutVar (Qualified (Just (ModuleName mn)) (Ident id))
    | mn == moduleName, ident == id ->
      Just []
  0, NeutStop (Qualified (Just (ModuleName mn)) (Ident id))
    | mn == moduleName, ident == id ->
      Just []
  0, SemRef (EvalExtern (Qualified (Just (ModuleName mn)) (Ident id))) [] _
    | mn == moduleName, ident == id ->
      Just []
  arity, NeutApp fn args
    | arity > 0, Just [] <- helper' shouldForce moduleName ident 0 fn ->
      if Array.length args >= arity
        then Just args
        else Nothing
  arity, SemRef (EvalExtern (Qualified (Just (ModuleName mn)) (Ident id))) [ ExternApp args ] _
    | mn == moduleName, ident == id ->
      if Array.length args >= arity
        then Just args
        else Nothing
  arity, SemRef (EvalExtern _) _ value | shouldForce ->
    helper' false moduleName ident arity (spy "forced" (force value))
  _, _ -> Nothing

data_array_indexImpl :: ForeignSemantics
data_array_indexImpl = Tuple (qualified "Data.Array" "indexImpl") go
  where
  go env _ = case _ of
    [ ExternUncurriedApp [ just, nothing, arr, ix ] ] ->
      Just $ makeLet Nothing arr \arr' ->
        makeLet Nothing ix \ix' ->
          SemBranch
            ( NonEmptyArray.singleton $ SemConditional
                ( Lazy.defer \_ ->
                    evalPrimOp env $ Op2 OpBooleanAnd
                      (evalPrimOp env (Op2 (OpIntOrd OpGte) ix' (liftInt 0)))
                      (evalPrimOp env (Op2 (OpIntOrd OpLt) ix' (evalPrimOp env (Op1 OpArrayLength arr'))))
                )
                ( Lazy.defer \_ ->
                    evalApp env just
                      [ evalPrimOp env (Op2 OpArrayIndex arr' ix') ]
                )
            )
            (pure nothing)
    _ ->
      Nothing


viewList :: BackendSemantics -> Maybe (Array BackendSemantics)
viewList = viewList' >>> case _ of
  Just { head, tail: Nothing } -> Just head
  _ -> Nothing
viewList' :: BackendSemantics -> Maybe { head :: Array BackendSemantics, tail :: Maybe BackendSemantics }
viewList' = go [] where
  typesMod = "Erl.Data.List.Types"
  listPrim = helper typesMod

  go acc s = case unit of
    _ | Just [a, as] <- listPrim "cons" 2 s ->
      go (acc <> [a]) as
    _ | Just [] <- listPrim "nil" 0 s ->
      Just { head: acc, tail: Nothing }
    -- TODO: use NeutStop to choose a different normal form for this?
    _ | Just [cons, nil, ls] <- helper "Data.Foldable" "foldrArray" 3 s
      , Just [] <- listPrim "cons" 0 cons
      , Just [] <- listPrim "nil" 0 nil
      , NeutLit (LitArray acc') <- ls ->
      Just { head: acc <> acc', tail: Nothing }
    _ | [] <- acc ->
      Nothing
    _ ->
      Just { head: acc, tail: Just s }
viewList'' :: BackendSemantics -> { head :: Array BackendSemantics, tail :: Maybe BackendSemantics }
viewList'' = fromMaybe <<< { head: [], tail: _ } <<< Just <*> viewList'

mkList :: Array BackendSemantics -> BackendSemantics
mkList = flip mkList'
  (NeutVar (qualified "Erl.Data.List.Types" "nil"))

mkList' :: Array BackendSemantics -> BackendSemantics -> BackendSemantics
mkList' = flip $ Array.foldr
  (\a as -> NeutApp (NeutVar (qualified "Erl.Data.List.Types" "cons")) [a, as])

mkList'' :: Array BackendSemantics -> Maybe BackendSemantics -> BackendSemantics
mkList'' = flip (maybe mkList (flip mkList'))

erl_data_list_types_appendImpl :: ForeignSemantics
erl_data_list_types_appendImpl = Tuple (qualified "Erl.Data.List.Types" "appendImpl") go
  where
  go _env _ = case _ of
    [ ExternApp [ l, r ] ]
      | Just ls <- viewList l ->
        let rs = viewList'' r in
        Just (mkList'' (ls <> rs.head) rs.tail)
      | otherwise -> spyWith "other" (const l) Nothing
    _ ->
      Nothing

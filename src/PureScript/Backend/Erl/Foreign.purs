module PureScript.Backend.Erl.Foreign ( erlForeignSemantics ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lazy (force)
import Data.Lazy as Lazy
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import PureScript.Backend.Optimizer.CoreFn (ConstructorType(..), Ident(..), Literal(..), ModuleName(..), Prop(..), ProperName(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (BackendSemantics(..), EvalRef(..), ExternSpine(..), SemConditional(..), evalApp, evalPrimOp, liftInt, makeLet)
import PureScript.Backend.Optimizer.Semantics.Foreign (ForeignEval, ForeignSemantics, qualified)
import PureScript.Backend.Optimizer.Syntax (BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorOrd(..))

erlForeignSemantics :: Map (Qualified Ident) ForeignEval
erlForeignSemantics = Map.fromFoldable $
  [ data_array_indexImpl
  , erl_data_list_types_appendImpl
  , erl_data_list_types_uncons
  , erl_data_tuple_fst
  , erl_data_tuple_snd
  ] <> erl_data_tuple

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
    helper' false moduleName ident arity (force value)
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


-- View a concrete list literal
viewList :: BackendSemantics -> Maybe (Array BackendSemantics)
viewList = viewList' >>> case _ of
  Just { init, tail: Nothing } -> Just init
  _ -> Nothing
-- View a list literal with a non-empty amount of elements consed on the front
viewList' :: BackendSemantics -> Maybe { init :: Array BackendSemantics, tail :: Maybe BackendSemantics }
viewList' = go [] where
  typesMod = "Erl.Data.List.Types"
  listPrim = helper typesMod

  go acc s = case unit of
    _ | Just [a, as] <- listPrim "cons" 2 s ->
      go (acc <> [a]) as
    _ | Just [] <- listPrim "nil" 0 s ->
      Just { init: acc, tail: Nothing }
    -- TODO: use NeutStop to choose a different normal form for this?
    _ | Just [cons, nil, ls] <- helper "Data.Foldable" "foldrArray" 3 s
      , Just [] <- listPrim "cons" 0 cons
      , Just [] <- listPrim "nil" 0 nil
      , NeutLit (LitArray acc') <- ls ->
      Just { init: acc <> acc', tail: Nothing }
    _ | Just [foldableArray, ls] <- helper "Erl.Data.List" "fromFoldable" 2 s
      -- , Just [] <- helper "Data.Foldable" "foldableArray" 0 foldableArray
      , NeutLit (LitArray acc') <- ls ->
      Just { init: acc <> acc', tail: Nothing }
    _ | [] <- acc ->
      Nothing
    _ ->
      Just { init: acc, tail: Just s }
-- View a list literal with a possibly empty amount of elements consed on the front
viewList'' :: BackendSemantics -> { init :: Array BackendSemantics, tail :: Maybe BackendSemantics }
viewList'' = fromMaybe <<< { init: [], tail: _ } <<< Just <*> viewList'

-- Make a concrete list literal
mkList :: Array BackendSemantics -> BackendSemantics
mkList = flip mkList'
  (NeutVar (qualified "Erl.Data.List.Types" "nil"))

-- Make a list literal with a tail
mkList' :: Array BackendSemantics -> BackendSemantics -> BackendSemantics
mkList' = flip $ Array.foldr
  (\a as -> NeutApp (NeutVar (qualified "Erl.Data.List.Types" "cons")) [a, as])

-- Make a list literal that maybe has a tail
mkList'' :: Array BackendSemantics -> Maybe BackendSemantics -> BackendSemantics
mkList'' = flip (maybe mkList (flip mkList'))

erl_data_list_types_appendImpl :: ForeignSemantics
erl_data_list_types_appendImpl = Tuple (qualified "Erl.Data.List.Types" "appendImpl")
  \_env qual -> case _ of
    [ ExternApp [ l, r ] ]
      | Just ls <- viewList' l ->
        case ls of
          { init, tail: Nothing } ->
            let rs = viewList'' r in
            Just (mkList'' (init <> rs.init) rs.tail)
          { init, tail: Just tail } ->
            Just (mkList' init (NeutApp (NeutVar qual) [ tail, r ]))
      | otherwise -> Nothing
    _ ->
      Nothing

ctorArgs :: Array BackendSemantics -> Array (Tuple String BackendSemantics)
ctorArgs = mapWithIndex \i -> Tuple ("value" <> show i)

ctor :: ConstructorType -> String -> String -> String -> Array BackendSemantics -> BackendSemantics
ctor ctorType modName tyName ctorName =
  NeutData (qualified modName ctorName) ctorType (ProperName tyName) (Ident ctorName) <<< ctorArgs

mkJust :: BackendSemantics -> BackendSemantics
mkJust = ctor SumType "Data.Maybe" "Maybe" "Just" <<< pure
mkNothing :: BackendSemantics
mkNothing = ctor SumType "Data.Maybe" "Maybe" "Nothing" []

erl_data_list_types_uncons :: ForeignSemantics
erl_data_list_types_uncons = Tuple (qualified "Erl.Data.List.Types" "uncons")
  let mkResult head tail = NeutLit (LitRecord [Prop "head" head, Prop "tail" tail]) in
  \_env _qual -> case _ of
    [ ExternApp [ list ] ]
      | Just [head, tail] <- helper "Erl.Data.List.Types" "cons" 2 list ->
        Just (mkJust (mkResult head tail))
      | Just [] <- helper "Erl.Data.List.Types" "nil" 0 list ->
        Just mkNothing
      | Just items <- viewList list ->
        Just case Array.uncons items of
          Nothing -> mkNothing
          Just { head, tail } -> mkJust (mkResult head (mkList tail))
      | otherwise -> Nothing
    _ ->
      Nothing

erl_data_tuple :: Array ForeignSemantics
erl_data_tuple = [1,2,3,4,5,6,7,8,9,10] <#> \n -> Tuple (qualified "Erl.Data.Tuple" ("uncurry" <> show n))
  \env _qual -> case _ of
    [ ExternApp [ fn, tuple ] ]
      | Just tupled <- helper "Erl.Data.Tuple" ("tuple" <> show n) n tuple ->
        Just (evalApp env fn tupled)
    _ -> Nothing

erl_data_tuple_fst :: ForeignSemantics
erl_data_tuple_fst = Tuple (qualified "Erl.Data.Tuple" "fst")
  \_env _qual -> case _ of
    [ ExternApp [ tuple ] ]
      | Just [l, _r] <- helper "Erl.Data.Tuple" "tuple2" 2 tuple ->
        Just l
    _ -> Nothing

erl_data_tuple_snd :: ForeignSemantics
erl_data_tuple_snd = Tuple (qualified "Erl.Data.Tuple" "snd")
  \_env _qual -> case _ of
    [ ExternApp [ tuple ] ]
      | Just [_l, r] <- helper "Erl.Data.Tuple" "tuple2" 2 tuple ->
        Just r
    _ -> Nothing

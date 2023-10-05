module PureScript.Backend.Erl.Foreign ( erlForeignSemantics ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lazy (force)
import Data.Lazy as Lazy
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.Backend.Optimizer.CoreFn (ConstructorType(..), Ident(..), Literal(..), ModuleName(..), Prop(..), ProperName(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (BackendSemantics(..), Env, EvalRef(..), ExternSpine(..), SemConditional(..), Spine, evalApp, evalPrimOp, liftInt, makeLet)
import PureScript.Backend.Optimizer.Semantics.Foreign (ForeignEval, ForeignSemantics, qualified)
import PureScript.Backend.Optimizer.Syntax (BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorOrd(..))

erlForeignSemantics :: Map (Qualified Ident) ForeignEval
erlForeignSemantics = Map.fromFoldable $
  [ data_array_indexImpl
  , erl_data_list_types_appendImpl
  , erl_data_list_types_uncons
  , erl_data_tuple_fst
  , erl_data_tuple_snd
  ] <> join
  [ erl_data_tuple_uncurryN
  , erl_atom
  , recognizeCoercions
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
    helper' false moduleName ident arity (force value)
  _, _ -> Nothing

handler :: String -> String -> Int -> (Env -> Spine BackendSemantics -> Maybe BackendSemantics) -> ForeignSemantics
handler moduleName ident arity f = Tuple (qualified moduleName ident)
  \env _qual -> case _ of
    [ ExternApp as ] | Array.length as >= arity -> f env as
    _ -> Nothing

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
erl_data_list_types_appendImpl = handler "Erl.Data.List.Types" "appendImpl" 2
  \_env -> case _ of
    [ l, r ]
      | Just ls <- viewList' l ->
        case ls of
          { init, tail: Nothing } ->
            let rs = viewList'' r in
            Just (mkList'' (init <> rs.init) rs.tail)
          { init, tail: Just tail } ->
            Just (mkList' init (NeutApp (NeutVar (qualified "Erl.Data.List.Types" "appendImpl")) [ tail, r ]))
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
erl_data_list_types_uncons =
  let mkResult head tail = NeutLit (LitRecord [Prop "head" head, Prop "tail" tail]) in
  handler "Erl.Data.List.Types" "uncons" 1 \_env -> case _ of
    [ list ]
      | Just [head, tail] <- helper "Erl.Data.List.Types" "cons" 2 list ->
        Just (mkJust (mkResult head tail))
      | Just [] <- helper "Erl.Data.List.Types" "nil" 0 list ->
        Just mkNothing
      | Just items <- viewList list ->
        Just case Array.uncons items of
          Nothing -> mkNothing
          Just { head, tail } -> mkJust (mkResult head (mkList tail))
    _ -> Nothing

erl_data_tuple_uncurryN :: Array ForeignSemantics
erl_data_tuple_uncurryN = [1,2,3,4,5,6,7,8,9,10] <#> \n ->
  handler "Erl.Data.Tuple" ("uncurry" <> show n) 2 \env -> case _ of
    [ fn, tuple ]
      | Just tupled <- helper "Erl.Data.Tuple" ("tuple" <> show n) n tuple ->
        Just (evalApp env fn tupled)
    _ -> Nothing

erl_data_tuple_fst :: ForeignSemantics
erl_data_tuple_fst = handler "Erl.Data.Tuple" "fst" 1
  \_env -> case _ of
    [ tuple ]
      | Just [l, _r] <- helper "Erl.Data.Tuple" "tuple2" 2 tuple ->
        Just l
    _ -> Nothing

erl_data_tuple_snd :: ForeignSemantics
erl_data_tuple_snd = handler "Erl.Data.Tuple" "snd" 1
  \_env -> case _ of
    [ tuple ]
      | Just [_l, r] <- helper "Erl.Data.Tuple" "tuple2" 2 tuple ->
        Just r
    _ -> Nothing

erl_atom :: Array ForeignSemantics
erl_atom =
  [ erl_atom_atom
  , erl_atom_toString
  , erl_atom_eqImpl
  ]
  where
  mn = "Erl.Atom"
  erl_atom_atom :: ForeignSemantics
  erl_atom_atom = handler mn "atom" 1
    \_env -> case _ of
      [ toString ]
        | Just [s] <- helper mn "toString" 1 toString ->
          Just s
      _ -> Nothing

  erl_atom_toString :: ForeignSemantics
  erl_atom_toString = handler mn "toString" 1
    \_env -> case _ of
      [ atom ]
        | Just [s] <- helper mn "atom" 1 atom ->
          Just s
      _ -> Nothing

  erl_atom_eqImpl :: ForeignSemantics
  erl_atom_eqImpl = handler mn "eqImpl" 2
    \_env -> case _ of
      [ atom1, atom2 ]
        | Just [NeutLit (LitString s1)] <- helper mn "atom" 1 atom1
        , Just [NeutLit (LitString s2)] <- helper mn "atom" 1 atom2 ->
          Just (NeutLit (LitBoolean (s1 == s2)))
      _ -> Nothing


coercions :: Array (String /\ String)
coercions = join
  let dir prefix items = lmap (prefix <> _) <$> items in
  [ dir "Erl.Data.Binary."
    [ "IOList" /\ "concat"
    , "IOData" /\ "fromIOList"
    , "IOData" /\ "fromBinary"
    , "IOData" /\ "fromString"
    , "IOData" /\ "concat"
    ]
  , [ "Erl.Data.Bitstring" /\ "fromBinary"
    ]
  ]

recognizeCoercions :: Array ForeignSemantics
recognizeCoercions = coercions <#> \(mn /\ id) ->
  handler mn id 1 (const only1)

only1 :: forall a. Array a -> Maybe a
only1 [a] = Just a
only1 _ = Nothing

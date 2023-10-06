module PureScript.Backend.Erl.Foreign ( erlForeignSemantics ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Filterable (filterMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lazy (force)
import Data.Lazy as Lazy
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.Backend.Erl.Calling (arg, arg', argMatch, curried, fosem, fosems, func, func', getEnv, many, uncurried)
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
data_array_indexImpl = fosem $ func "Data.Array.indexImpl" $
  uncurried ado
    just <- arg
    nothing <- arg
    arr <- arg
    ix <- arg
    env <- getEnv
    in
      makeLet Nothing arr \arr' ->
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
erl_data_list_types_appendImpl = fosem $ func "Erl.Data.List.Types.appendImpl" $
  curried ado
    ls <- filterMap viewList' arg
    r <- arg
    in case ls of
      { init, tail: Nothing } ->
        let rs = viewList'' r in
        mkList'' (init <> rs.init) rs.tail
      { init, tail: Just tail } ->
        mkList' init (NeutApp (NeutVar (qualified "Erl.Data.List.Types" "appendImpl")) [ tail, r ])

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
erl_data_list_types_uncons = fosems "Erl.Data.List.Types.uncons"
  let mkResult head tail = NeutLit (LitRecord [Prop "head" head, Prop "tail" tail]) in
  [ argMatch $ func' "Erl.Data.List.Types.cons" ado
      head <- arg
      tail <- arg
      in mkJust (mkResult head tail)
  , argMatch $ func' "Erl.Data.List.Types.nil" ado
      in mkNothing
  , ado
      items <- filterMap viewList arg
      in case Array.uncons items of
        Nothing -> mkNothing
        Just { head, tail } -> mkJust (mkResult head (mkList tail))
  ]

erl_data_tuple_uncurryN :: Array ForeignSemantics
erl_data_tuple_uncurryN = [1,2,3,4,5,6,7,8,9,10] <#> \n -> fosem $
  func ("Erl.Data.Tuple.uncurry" <> show n) ado
    fn <- arg
    tupled <- argMatch $ func' ("Erl.Data.Tuple.tuple" <> show n) $
      many $ Array.replicate n Just
    env <- getEnv
    in evalApp env fn tupled

erl_data_tuple_fst :: ForeignSemantics
erl_data_tuple_fst = fosem $ func "Erl.Data.Tuple.fst" do
  argMatch $ func' "Erl.Data.Tuple.tuple2" $ arg <* arg

erl_data_tuple_snd :: ForeignSemantics
erl_data_tuple_snd = fosem $ func "Erl.Data.Tuple.snd" do
  argMatch $ func' "Erl.Data.Tuple.tuple2" $ arg *> arg

erl_atom :: Array ForeignSemantics
erl_atom =
  [ erl_atom_atom
  , erl_atom_toString
  , erl_atom_eqImpl
  ]
  where
  erl_atom_atom :: ForeignSemantics
  erl_atom_atom = fosem $ func "Erl.Atom.atom" $ argMatch $ func' "Erl.Atom.toString" $ arg

  erl_atom_toString :: ForeignSemantics
  erl_atom_toString = fosem $ func "Erl.Atom.toString" $ argMatch $ func' "Erl.Atom.atom" $ arg

  literal = argMatch $ func' "Erl.Atom.atom" $ arg'
    case _ of NeutLit (LitString s) -> Just s

  erl_atom_eqImpl :: ForeignSemantics
  erl_atom_eqImpl = fosem $ func "Erl.Atom.eqImpl" ado
    s1 <- literal
    s2 <- literal
    in NeutLit (LitBoolean (s1 == s2))

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

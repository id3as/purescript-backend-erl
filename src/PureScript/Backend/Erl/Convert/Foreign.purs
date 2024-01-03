module PureScript.Backend.Erl.Convert.Foreign ( codegenForeign ) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Data.Array as A
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (bimap)
import Data.Maybe (Maybe(..), fromMaybe', maybe)
import Data.Newtype (unwrap)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import PureScript.Backend.Erl.Calling (CallPS(..), Conventions, Converter, Converters, applyConventions, arg', argMatch, callConverters, callErl, callPS, codegenArg, converts', func, indexPatterns, noArgs, qualErl, qualPS, thunkErl, withEnv)
import PureScript.Backend.Erl.Convert.Common (toErlVarPat)
import PureScript.Backend.Erl.Syntax (ErlExpr, FunHead(..))
import PureScript.Backend.Erl.Syntax as S
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (NeutralExpr(..))
import PureScript.Backend.Optimizer.Syntax (BackendSyntax(..))

codegenForeign :: (NeutralExpr -> ErlExpr) -> NeutralExpr -> Maybe ErlExpr
codegenForeign codegenExpr s = case unwrap s of
  Var _ -> codegenForeign' codegenExpr s
  App (NeutralExpr (Var _)) _ -> codegenForeign' codegenExpr s
  _ -> Nothing

codegenForeign' :: (NeutralExpr -> ErlExpr) -> NeutralExpr -> Maybe ErlExpr
codegenForeign' codegenExpr s
  | Just result <- converts' converters codegenExpr s =
    Just result
  | Just result <- codegenList codegenExpr s =
    Just result
  | otherwise =
    Nothing

converters :: Converters
converters = indexPatterns (tupleCalls <> specificCalls) <> applyConventions ffiSpecs

tupleCalls :: Array Converter
tupleCalls = [1,2,3,4,5,6,7,8,9,10] >>= \arity ->
  -- `tupleN` with N arguments applied
  [ func ("Erl.Data.Tuple.tuple" <> show arity) $ S.Tupled <$> sequence (Array.replicate arity codegenArg)
  -- `uncurryN` with 1 argument applied
  , func ("Erl.Data.Tuple.uncurry" <> show arity) $ withEnv $ arg' case _ of
      NeutralExpr (Abs a e) | NEA.length a >= arity -> \codegenExpr ->
        S.Fun Nothing $ Array.singleton $ Tuple (FunHead [ S.MatchTuple (toErlVarPat <$> NEA.take arity a) ] Nothing) $
          codegenExpr (maybe e (\a' -> NeutralExpr (Abs a' e)) (NEA.fromArray $ NEA.drop arity a))
  ]

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

specificCalls :: Array Converter
specificCalls =
  [ func "Erl.Atom.atom" $ arg' \(NeutralExpr (Lit (LitString s))) -> S.Literal (S.Atom s)
  , func "Data.Unit.unit" noArgs $> S.Literal (S.Atom "unit")
  , func "Erl.Data.Binary.Type.mempty_" noArgs $> S.Literal (S.String "")
  , func "Erl.Data.Binary.IOData.mempty_" noArgs $> S.List []
  , func "Erl.Data.Binary.IOList.mempty_" noArgs $> S.List []
  , func "Erl.Data.Binary.Type.append_" $ lift2 S.BinaryAppend codegenArg codegenArg
  , func "Erl.Data.Binary.IOData.append_" $ S.List <$> sequence [ codegenArg, codegenArg ]
  , func "Erl.Data.Binary.IOList.append_" $ S.List <$> sequence [ codegenArg, codegenArg ]
  , func "Erl.Data.Binary.IOList.fromBinary" $ S.List <$> sequence [ codegenArg ]
  , func "Erl.Data.List.Types.null" $ S.BinOp S.IdenticalTo (S.List []) <$> codegenArg
  , func "Erl.Data.Map.empty" $ noArgs $> S.Map []
  , func "Erl.Data.Map.isEmpty" $ S.BinOp S.IdenticalTo (S.Map []) <$> codegenArg
  , func "Erl.Data.Map.insert" ado
      key <- codegenArg
      value <- codegenArg
      container <- codegenArg
      in S.MapUpdate container [ Tuple key value ]

  , func "Erl.Data.Map.fromFoldable" ado
      _ <- argMatch $ func "Data.List.Types.foldableList" noArgs
      list <- withEnv $ arg' \l ->
        (\items codegen -> join bimap codegen <$> items) $
          unTuple <$> gatherList l
      in S.Map list

  -- Needs some mechanism for fresh names :(
  -- , func "Erl.Data.Variant.matchImpl" $ partial ado
  --     fns <- arg'' \(NeutralExpr (Lit (LitRecord fns))) -> NEA.fromArray fns
  --     variant <- codegenArg
  --     codegenExpr <- getEnv
  --     in S.Case variant $ fns <#> \(Prop variantType fn) ->
  --       S.CaseClause (S.MatchMap [Tuple "type" (S.MatchLiteral (S.Atom variantType)), Tuple "value" (S.BindVar "Value")]) Nothing
  --       do S.curriedApp (codegenExpr fn) [S.Var "Value"]
  -- , func "Erl.Data.Variant.matchImpl" $ partial ado
  --     NeutralExpr (Lit (LitRecord fns)) <- arg
  --     codegenExpr <- getEnv
  --     in S.Fun Nothing $ fns <#> \(Prop variantType fn) -> Tuple
  --       do S.FunHead [S.MatchMap [Tuple "type" (S.MatchLiteral (S.Atom variantType)), Tuple "value" (S.BindVar "Value")]] Nothing
  --       do S.curriedApp (codegenExpr fn) [S.Var "Value"]
  -- , func "Erl.Data.Variant.matchImpl" ado
  --     fns <- codegenArg
  --     in S.Fun Nothing $ pure $ Tuple
  --       do S.FunHead [S.MatchMap [Tuple "type" (S.BindVar "Type"), Tuple "value" (S.BindVar "Value")]] Nothing
  --       do S.curriedApp (S.qualified "erlang" "map_get" [S.Var "Type", fns]) [S.Var "Value"]

  , func "Partial._unsafePartial" $ S.curriedApp <$> codegenArg <@> [S.atomLiteral "unit"]
  ]

gatherList :: Partial => NeutralExpr -> Array NeutralExpr
gatherList (NeutralExpr (CtorSaturated (Qualified (Just (ModuleName "Data.List.Types")) (Ident ctor)) _ _ _ fields)) =
  case ctor, fields of
    "Nil", [] -> []
    "Cons", [Tuple _ a, Tuple _ as] -> A.cons a (gatherList as)

unTuple :: Partial => NeutralExpr -> Tuple NeutralExpr NeutralExpr
unTuple (NeutralExpr (CtorSaturated (Qualified (Just (ModuleName "Data.Tuple")) (Ident "Tuple")) _ _ _ [Tuple _ l, Tuple _ r])) = Tuple l r

ffiSpecs :: Conventions
ffiSpecs = callConverters
  let
    -- Note: this does not handle swapping orders of arguments ... yet
    calling arity call ps erl =
      { ps: qualPS ps, arity, erl: qualErl erl, call }
    pure1 = calling
      (callPS (Curried (NEA.singleton unit)))
      (callErl [unit])
    pure2 = calling
      (callPS (Curried (NEA.singleton unit <> NEA.singleton unit)))
      (callErl [unit, unit])
    eff1 = calling
      (callPS (Curried (NEA.singleton unit)))
      (callErl [unit] <|> thunkErl)
  in
    [ pure1 "Erl.Data.Bitstring.isBinary" "erlang:is_binary"
    , pure1 "Erl.Data.Bitstring.bitSize" "erlang:bit_size"
    , pure1 "Erl.Data.Bitstring.byteSize" "erlang:byte_size"
    , pure1 "Erl.Data.Binary.byteSize" "erlang:byte_size"
    , pure1 "Erl.Data.Binary.IOList.byteSize" "erlang:iolist_size"
    , pure1 "Erl.Data.Binary.IOData.byteSize" "erlang:iolist_size"
    , pure1 "Erl.Data.Binary.IOList.toBinary" "erlang:iolist_to_binary"
    , pure1 "Erl.Data.Binary.IOData.toBinary" "erlang:iolist_to_binary"
    , pure1 "Erl.Kernel.Erlang.listToBinary" "erlang:list_to_binary"
    , pure1 "Erl.Atom.atom" "erlang:binary_to_atom"
    , pure1 "Erl.Atom.toString" "erlang:atom_to_binary"
    , pure1 "Partial._crashWith" "erlang:error"
    -- Effectful, thus wrapped in a thunk
    , eff1 "Erl.Kernel.Exceptions.throw" "erlang:throw"
    , eff1 "Erl.Kernel.Exceptions.error" "erlang:error"
    , eff1 "Erl.Kernel.Exceptions.exit" "erlang:exit"
    , eff1 "Effect.Exception.throwException" "erlang:error"

    , pure2 "Erl.Data.List.Types.mapImpl" "lists:map"
    , pure2 "Erl.Data.List.Types.filter" "lists:filter"

    , pure1 "Erl.Data.Map.values" "maps:values"
    , pure1 "Erl.Data.Map.keys" "maps:keys"
    , pure1 "Erl.Data.Map.size" "maps:size"
    , pure2 "Erl.Data.Map.union" "maps:merge"
    , pure2 "Erl.Data.Map.mapWithKeyImpl" "maps:map"
    , pure2 "Erl.Data.Map.member" "maps:is_key"
    , pure2 "Erl.Data.Map.delete" "maps:remove"
    ]

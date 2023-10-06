module PureScript.Backend.Erl.Convert.Foreign ( codegenForeign ) where

import Prelude

import Control.Apply (lift2)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), fromMaybe', maybe)
import Data.Newtype (unwrap)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import PureScript.Backend.Erl.Calling (ArityErl, ArityPS, CallErl(..), CallPS(..), CallingErl(..), CallingPS(..), Converter, GlobalErl, arg', callConverters, codegenArg, converts, func, noArgs, qualErl, qualPS, withEnv)
import PureScript.Backend.Erl.Convert.Common (toErlVarExpr)
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
  | Just result <- codegenList codegenExpr s =
    Just result
  | Just result <- converts specificCalls codegenExpr s =
    Just result
  | Just result <- converts tupleCalls codegenExpr s =
    Just result
  | Just result <- callConverters ffiSpecs codegenExpr s =
    Just result
  | otherwise =
    Nothing

tupleCalls :: Array Converter
tupleCalls = [1,2,3,4,5,6,7,8,9,10] >>= \arity ->
  -- `tupleN` with N arguments applied
  [ func ("Erl.Data.Tuple.tuple" <> show arity) $ S.Tupled <$> sequence (Array.replicate arity codegenArg)
  -- `uncurryN` with 1 argument applied
  , func ("Erl.Data.Tuple.uncurry" <> show arity) $ withEnv $ arg' case _ of
      NeutralExpr (Abs a e) | NEA.length a >= arity -> \codegenExpr ->
        S.Fun Nothing $ Array.singleton $ Tuple (FunHead [ S.Tupled (toErlVarExpr <$> NEA.take arity a) ] Nothing) $
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
  ]

ffiSpecs :: Array { ps :: Qualified Ident, arity :: ArityPS, erl :: GlobalErl, call :: ArityErl }
ffiSpecs =
  let
    -- Note: this does not handle swapping orders of arguments ... yet
    calling arity call ps erl =
      { ps: qualPS ps, arity, erl: qualErl erl, call }
    pure1 = calling
      (CallingPS BasePS (Curried (NEA.singleton unit)))
      (CallingErl (NEA.singleton (Call [unit])))
    eff1 = calling
      (CallingPS (CallingPS BasePS (Curried (NEA.singleton unit))) (UncurriedEffect []))
      (CallingErl (NEA.singleton (Call [unit])))
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
    -- Effectful, thus wrapped in a thunk
    , eff1 "Erl.Kernel.Exceptions.throw" "erlang:throw"
    , eff1 "Erl.Kernel.Exceptions.error" "erlang:error"
    , eff1 "Erl.Kernel.Exceptions.exit" "erlang:exit"
    , eff1 "Effect.Exception.throwException" "erlang:error"
    ]

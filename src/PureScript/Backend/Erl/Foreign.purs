module PureScript.Backend.Erl.Foreign where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Enum (fromEnum)
import Data.Lazy as Lazy
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PureScript.Backend.Optimizer.CoreFn (Ident, Literal(..), Qualified)
import PureScript.Backend.Optimizer.Semantics (BackendSemantics(..), ExternSpine(..), SemConditional(..), evalApp, evalPrimOp, liftInt, makeLet)
import PureScript.Backend.Optimizer.Semantics.Foreign (ForeignEval, ForeignSemantics, qualified)
import PureScript.Backend.Optimizer.Syntax (BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorOrd(..))
import PureScript.Backend.Optimizer.Utils (foldr1Array)

erlForeignSemantics :: Map (Qualified Ident) ForeignEval
erlForeignSemantics = Map.fromFoldable
  [ data_array_indexImpl
  ]

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

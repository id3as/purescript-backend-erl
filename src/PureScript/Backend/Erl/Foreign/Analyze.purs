module PureScript.Backend.Erl.Foreign.Analyze where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import PureScript.Backend.Erl.Calling (qualPS)
import PureScript.Backend.Optimizer.Analysis (BackendAnalysis(..), Complexity(..), ResultTerm(..), analysisOf, analyzeDefault, bump, withResult)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (BackendExpr(..), Ctx)
import PureScript.Backend.Optimizer.Syntax (BackendSyntax(..))

type Analyzer = Ctx -> BackendSyntax BackendExpr -> Maybe BackendAnalysis

analyzeCustom :: Array Analyzer -> Ctx -> BackendSyntax BackendExpr -> Maybe BackendAnalysis
analyzeCustom custom ctx = case _ of
  expr | Just analysis <- Array.findMap (\f -> f ctx expr) (custom <> analyses) ->
    Just analysis
  expr
    | Just [ExprSyntax _ inst, v@(ExprSyntax _ (Lit (LitArray _)))] <- helper "Erl.Data.List" "fromFoldable" 2 expr
    , Just _ <- helper "Data.Foldable" "foldableArray" 0 inst ->
      Just $ constructing ctx [v]
    | Just [ExprSyntax _ cons, ExprSyntax _ nil, ExprSyntax s (Lit (LitArray _))] <- helper "Data.Foldable" "foldrArray" 3 expr
    , Just _ <- helper "Erl.Data.List.Types" "cons" 0 cons
    , Just _ <- helper "Erl.Data.List.Types" "nil" 0 nil ->
      Just $ withResult KnownNeutral s
  expr
    | Just [ExprSyntax s (Lit (LitString _))] <- helper "Erl.Atom" "atom" 1 expr ->
      Just $ withResult KnownNeutral s
  _ -> Nothing

analyses :: Array (Ctx -> BackendSyntax BackendExpr -> Maybe BackendAnalysis)
analyses =
  [ asConstructor "Erl.Data.List.Types" "cons" 2
  , asConstructor "Erl.Data.List.Types" "nil" 0
  ] <> join
  [ [1,2,3,4,5,6,7,8,9,10] <#> \n -> asConstructor "Erl.Data.Tuple" ("tuple" <> show n) n
  ]

-- no `usedDep` since the constructor will get inlined by
-- `PureScript.Backend.Erl.Convert.Foreign`
asConstructor :: String -> String -> Int -> Ctx -> BackendSyntax BackendExpr -> Maybe BackendAnalysis
asConstructor mn id arity ctx expr =
  constructing ctx <$> helper mn id arity expr

constructing :: Ctx -> Array BackendExpr -> BackendAnalysis
constructing _ctx cs =
  withResult KnownNeutral
    $ bump
    $ foldMap analysisOf cs

helper :: String -> String -> Int -> BackendSyntax BackendExpr -> Maybe (Array BackendExpr)
helper moduleName ident = case _, _ of
  0, Var (Qualified (Just (ModuleName mn)) (Ident id))
    | mn == moduleName, ident == id ->
      Just []
  arity, App (ExprSyntax _ (Var (Qualified (Just (ModuleName mn)) (Ident id)))) args
    | mn == moduleName, ident == id ->
      if NEA.length args >= arity
        then Just (NEA.toArray args)
        else Nothing
  _, _ -> Nothing

neverInlineConstant :: String -> Analyzer
neverInlineConstant s = unsafePartial
  let Qualified (Just (ModuleName mod)) (Ident i) = qualPS s in
  \_ctx expr -> helper mod i 0 expr >>= \_ -> neverInlineAnalysis expr

neverInlineAnalysis :: BackendSyntax BackendExpr -> Maybe BackendAnalysis
neverInlineAnalysis expr =
  let BackendAnalysis { usages, args, deps, externs } = analyzeDefault expr
  in Just $ BackendAnalysis
    { usages
    , size: top
    , complexity: NonTrivial
    , args
    , rewrite: false
    , deps
    , result: Unknown
    , externs
    }

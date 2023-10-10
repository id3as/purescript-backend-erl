module PureScript.Backend.Erl.Convert where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Foldable (class Foldable, foldMap, foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Erl.Calling (CallPS(..), CallingPS(..), Conventions, GlobalErl(..), applyConventions, callAs, callAs', callErl, callPS)
import PureScript.Backend.Erl.Constants as C
import PureScript.Backend.Erl.Convert.Before (renameRoot)
import PureScript.Backend.Erl.Convert.Common (erlModuleNameForeign, erlModuleNamePs, tagAtom, toAtomName, toErlVar, toErlVarExpr, toErlVarName, toErlVarWith)
import PureScript.Backend.Erl.Convert.Foreign (codegenForeign)
import PureScript.Backend.Erl.Parser (ForeignDecls)
import PureScript.Backend.Erl.Syntax (ErlDefinition(..), ErlExport(..), ErlExpr, ErlModule, atomLiteral)
import PureScript.Backend.Erl.Syntax as S
import PureScript.Backend.Optimizer.Convert (BackendModule, BackendBindingGroup)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName, Prop(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (NeutralExpr)
import PureScript.Backend.Optimizer.Syntax (BackendAccessor(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Level(..), Pair(..))

type CodegenEnv =
  { currentModule :: ModuleName
  -- A local identifier can be replaced with an expression. This is used in
  -- the generation of LetRec, where recursive calls need to be modified
  , substitutions :: Map (Tuple (Maybe Ident) Level) ErlExpr
  , callingConventions :: Conventions
  }

codegenModule :: BackendModule -> ForeignDecls -> Conventions -> Tuple ErlModule Conventions
codegenModule { name: currentModule, bindings, imports, foreign: foreign_, exports: moduleExports } foreigns initialConventions =
  let
    Tuple thisModuleConventions thisModuleBindings = foldMap (codegenTopLevelBindingGroup currentModule) bindings
    withForeignConventions = initialConventions <> foldMap fst reexportForeigns
    callingConventions = withForeignConventions <> thisModuleConventions
    -- We do not qualify same-module bindings
    localConventions = thisModuleConventions <#> (map <<< map <<< lmap) (over GlobalErl _ { module = Nothing })
    localCallingConventions = withForeignConventions <> localConventions
    codegenEnv = { currentModule, substitutions: Map.empty, callingConventions: localCallingConventions }

    definitions :: Array ErlDefinition
    definitions = Array.concat
      [ thisModuleBindings <@> codegenEnv
      , reexportForeigns <#> snd
      ]

    reexportForeigns :: Array (Tuple Conventions ErlDefinition)
    reexportForeigns = foreigns.exported >>= \(Tuple decl arity) -> do
      guard $ Ident decl `Set.member` foreign_
      let
        vars =
          Array.replicate arity unit
            # mapWithIndex \i _ ->
              toErlVarExpr (Tuple Nothing (Level i))
        calling =
          case NEA.fromArray (void vars) of
            Nothing ->
              callAs' (Qualified (Just currentModule) (Ident decl)) BasePS
                (GlobalErl { module: Just $ erlModuleNameForeign currentModule, name: decl })
                (callErl []) Nothing
            Just vars' ->
              callAs' (Qualified (Just currentModule) (Ident decl)) (callPS (Curried vars'))
                (GlobalErl { module: Just $ erlModuleNameForeign currentModule, name: decl })
                (callErl (void vars)) Nothing
      pure $ Tuple calling $ FunctionDefinition decl [] $
        S.curriedFun vars $
          S.FunCall (Just (S.atomLiteral (erlModuleNameForeign currentModule))) (S.atomLiteral decl) vars

    exports :: Array ErlExport
    exports = definitionExports <$> definitions

  in
    Tuple
      { moduleName: erlModuleNamePs currentModule
      , definitions
      , exports
      , comments: []
      }
      callingConventions

definitionExports :: ErlDefinition -> ErlExport
definitionExports = case _ of
  FunctionDefinition f a _ ->
    Export f (Array.length a)

codegenTopLevelBindingGroup
  :: ModuleName
  -> BackendBindingGroup Ident NeutralExpr
  -> Tuple Conventions (Array (CodegenEnv -> ErlDefinition))
codegenTopLevelBindingGroup currentModule { bindings } =
  foldMap (codegenTopLevelBinding currentModule <<< map renameRoot) bindings

codegenTopLevelBinding
  :: ModuleName
  -> Tuple Ident NeutralExpr
  -> Tuple Conventions (Array (CodegenEnv -> ErlDefinition))
codegenTopLevelBinding currentModule (Tuple (Ident i) n) =
  case unwrap n of
    CtorDef _ _ tag fields | vars <- S.Var <<< toErlVarName <$> fields -> pure
      [ const $ FunctionDefinition i [] $
          S.curriedFun vars $
            S.Tupled $ [ tagAtom tag ] <> vars
      ]
    Abs vars e ->
      callThisAs (callPS (Curried (void vars))) (callErl (void (NEA.toArray vars))) Nothing
      let evars = locals vars in
      [ const $ FunctionDefinition i [] $ S.curriedFun evars $
          S.FunCall Nothing (S.Literal (S.Atom i)) evars
      , \codegenEnv -> FunctionDefinition i (uncurry toErlVar <$> NEA.toArray vars) $ codegenExpr codegenEnv e
      ]
    UncurriedAbs vars e | Array.length vars > 0 ->
      callThisAs (callPS (Uncurried (void vars))) (callErl (void vars)) Nothing
      let evars = locals vars in
      [ const $ FunctionDefinition i [] $ S.simpleFun evars $
          S.FunCall Nothing (S.Literal (S.Atom i)) evars
      , \codegenEnv -> FunctionDefinition i (uncurry toErlVar <$> vars) $ codegenExpr codegenEnv e
      ]
    UncurriedEffectAbs vars e | Array.length vars > 0 ->
      callThisAs (callPS (UncurriedEffect (void vars))) (callErl (void vars)) (Just (callErl []))
      let evars = locals vars in
      [ const $ FunctionDefinition i [] $ S.simpleFun evars $
          S.FunCall Nothing (S.Literal (S.Atom i)) evars
      , \codegenEnv -> FunctionDefinition i (uncurry toErlVar <$> vars) $ codegenChain effectChainMode codegenEnv e
      ]
    _ -> pure
      [ \codegenEnv -> FunctionDefinition i [] $ codegenExpr codegenEnv n ]
  where
  callThisAs x y z = Tuple (callAs (Qualified (Just currentModule) (Ident i)) x y z)

-- When we recurse in the function side of applications, we don't need to
-- consider calling conventions: they were already handled
codegenExpr0 :: CodegenEnv -> NeutralExpr -> ErlExpr
codegenExpr0 codegenEnv | Map.isEmpty (unwrap codegenEnv.callingConventions) = codegenExpr codegenEnv
codegenExpr0 codegenEnv = codegenExpr codegenEnv { callingConventions = mempty }

codegenExpr :: CodegenEnv -> NeutralExpr -> ErlExpr
codegenExpr codegenEnv@{ currentModule } s = case unwrap s of
  _ | Just result <- codegenForeign (codegenExpr codegenEnv) s ->
    result

  _ | Just result <- applyConventions codegenEnv.callingConventions (codegenExpr codegenEnv) s ->
    result

  -- Currently disabled due to this bug (OTP 26.1):
  --   Sub pass ssa_opt_type_start
  --   internal error in pass beam_ssa_opt:
  --   exception error: bad key
  --     in call from beam_ssa_type:opt_local_return/4 (beam_ssa_type.erl, line 733)
  --     in call from beam_ssa_type:opt_make_fun/4 (beam_ssa_type.erl, line 716)
  --     in call from beam_ssa_type:opt_is/8 (beam_ssa_type.erl, line 580)
  --     in call from beam_ssa_type:opt_bs/8 (beam_ssa_type.erl, line 527)
  --     in call from beam_ssa_type:do_opt_function/6 (beam_ssa_type.erl, line 505)
  --     in call from beam_ssa_type:opt_function/6 (beam_ssa_type.erl, line 478)
  --     in call from beam_ssa_type:opt_start_1/5 (beam_ssa_type.erl, line 96)
  Var (Qualified (Just mn) (Ident i)) | mn /= currentModule ->
    S.FunCall (Just (S.atomLiteral $ erlModuleNamePs mn)) (S.atomLiteral i) []

  Var (Qualified _ (Ident i)) ->
    S.FunCall Nothing (S.atomLiteral i) []

  Local i l | Just replacement <- Map.lookup (Tuple i l) codegenEnv.substitutions ->
    replacement
  Local i l ->
    S.Var $ toErlVar i l
  Lit l ->
    codegenLiteral codegenEnv l
  App f p ->
    S.curriedApp (codegenExpr0 codegenEnv f) (codegenExpr codegenEnv <$> p)
  Abs a e -> do
    S.curriedFun (toErlVarExpr <$> a) (codegenExpr codegenEnv e)
  UncurriedApp f p ->
    S.FunCall Nothing (codegenExpr0 codegenEnv f) (codegenExpr codegenEnv <$> p)
  UncurriedAbs a e ->
    S.simpleFun (toErlVarExpr <$> a) (codegenExpr codegenEnv e)
  UncurriedEffectApp f p ->
    S.thunk $ S.FunCall Nothing (codegenExpr0 codegenEnv f) (codegenExpr codegenEnv <$> p)
  UncurriedEffectAbs a e ->
    S.simpleFun (toErlVarExpr <$> a) (codegenChain effectChainMode codegenEnv e)
  Accessor e (GetProp i) ->
    S.FunCall (Just $ atomLiteral C.erlang) (atomLiteral C.map_get)
      [ S.atomLiteral i, codegenExpr codegenEnv e ]

  Accessor e (GetIndex i) ->
    S.FunCall (Just $ atomLiteral C.array) (atomLiteral C.get)
      [ S.numberLiteral i, codegenExpr codegenEnv e ]

  Accessor e (GetCtorField _ _ _ _ _ i) ->
    S.FunCall (Just $ atomLiteral C.erlang) (atomLiteral C.element)
      -- plus 1 for tag at beginning of tuple
      -- plus 1 for 1-indexing
      [ S.numberLiteral (i + 2), codegenExpr codegenEnv e ]

  Update e props ->
    S.MapUpdate (codegenExpr codegenEnv e) ((\(Prop name p) -> Tuple name (codegenExpr codegenEnv p)) <$> props)

  CtorSaturated _ _ _ tagName xs ->
    S.Tupled $ [ tagAtom tagName ] <> map (codegenExpr codegenEnv <<< snd) xs

  CtorDef _ _ _ _ ->
    unsafeCrashWith "codegenExpr:CtorDef - handled by codegenTopLevelBinding!"
  LetRec _lvl _bindings _e ->
    codegenPureChain codegenEnv s
  Let _i _lvl _e _e' ->
    codegenPureChain codegenEnv s

  Branch b o -> do
    let
      goPair :: Pair NeutralExpr -> Tuple ErlExpr ErlExpr
      goPair (Pair c e) = Tuple (codegenExpr codegenEnv c) (codegenExpr codegenEnv e)

      go (Tuple c e) ee | S.guardExpr c =
        case ee of
          S.If clauses ->
            S.If (S.IfClause c e NEA.: clauses)
          _ ->
            S.If (S.IfClause c e NEA.: NEA.singleton (S.IfClause (S.Literal $ S.Atom "true") ee))
      go (Tuple c e) ee =
        S.Case c
          ( S.CaseClause (S.atomLiteral C.true_) Nothing e NEA.:
              NEA.singleton (S.CaseClause (S.Var "_") Nothing ee)
          )

    foldr go (codegenExpr codegenEnv o) (goPair <$> b)

  EffectBind _ _ _ _ ->
    codegenEffectChain codegenEnv s
  EffectPure _ ->
    codegenEffectChain codegenEnv s
  EffectDefer _ ->
    codegenEffectChain codegenEnv s
  PrimEffect _ ->
    unsafeCrashWith "codegenExpr:PrimEffect - should have been filtered out"

  PrimOp o ->
    codegenPrimOp codegenEnv o
  PrimUndefined ->
    S.atomLiteral C.undefined

  Fail i ->
    S.FunCall (Just $ atomLiteral C.erlang) (atomLiteral C.throw) [
      S.Tupled [ S.atomLiteral "fail", S.stringLiteral i ]
    ]

codegenLiteral :: CodegenEnv -> Literal NeutralExpr -> ErlExpr
codegenLiteral codegenEnv = case _ of
  LitInt i -> S.Literal $ S.Integer i
  LitNumber n -> S.Literal $ S.Float n
  LitString s -> S.Literal $ S.String s
  LitChar c -> S.Literal $ S.Char c
  LitBoolean b -> S.Literal $ S.Atom $ if b then C.true_ else C.false_
  LitArray a ->
    S.FunCall (Just $ atomLiteral C.array) (atomLiteral C.from_list)
      [ S.List $ codegenExpr codegenEnv <$> a ]
  LitRecord r -> S.Map $ (\(Prop f e) -> Tuple f (codegenExpr codegenEnv e)) <$> r

type ChainMode = { effect :: Boolean }

pureChainMode :: ChainMode
pureChainMode = { effect: false }

effectChainMode :: ChainMode
effectChainMode = { effect: true }

codegenPureChain :: CodegenEnv -> NeutralExpr -> ErlExpr
codegenPureChain codegenEnv = codegenChain pureChainMode codegenEnv

codegenEffectChain :: CodegenEnv -> NeutralExpr -> ErlExpr
codegenEffectChain codegenEnv = S.thunk <<< codegenChain effectChainMode codegenEnv

codegenChain :: ChainMode -> CodegenEnv -> NeutralExpr -> ErlExpr
codegenChain chainMode codegenEnv0 = collect [] codegenEnv0.substitutions
  where
  codegenEnv = codegenEnv0 { substitutions = _ }

  -- `expression` has type `Effect ..`, so we can confidently unthunk here
  codegenEffectBind :: Map _ ErlExpr -> NeutralExpr -> ErlExpr
  codegenEffectBind substitutions expression =
    case unwrap expression of
      UncurriedEffectApp f p ->
        S.FunCall Nothing (codegenExpr (codegenEnv substitutions) f) (codegenExpr (codegenEnv substitutions) <$> p)
      _ ->
        S.unthunk $ codegenExpr (codegenEnv substitutions) expression

  finish :: Boolean -> Array _ -> Map _ ErlExpr -> NeutralExpr -> ErlExpr
  finish shouldUnthunk bindings substitutions expression = do
    let
      maybeUnthunk :: ErlExpr -> ErlExpr
      maybeUnthunk = if shouldUnthunk then S.unthunk' else identity
    S.assignments bindings $ maybeUnthunk $ codegenExpr (codegenEnv substitutions) expression

  collect :: Array _ -> Map _ ErlExpr -> NeutralExpr -> ErlExpr
  collect bindings substitutions expression = case unwrap expression of
    Let i l v e' ->
      collect (Array.snoc bindings $ Tuple (toErlVar i l) (codegenExpr (codegenEnv substitutions) v)) substitutions e'
    LetRec lvl recBindings e' ->
      let
        { bindings: moreBindings
        , substitutions: nextSubstitutions
        } = codegenLetRec (codegenEnv substitutions) { lvl, bindings: recBindings }
      in collect (bindings <> moreBindings) nextSubstitutions e'
    EffectPure e' | chainMode.effect ->
      finish false bindings substitutions e'
    EffectBind i l v e' | chainMode.effect ->
      collect
        (Array.snoc bindings $ Tuple (toErlVar i l) (codegenEffectBind substitutions v))
        substitutions
        e'
    EffectDefer e' | chainMode.effect ->
      collect bindings substitutions e'
    _ ->
      finish chainMode.effect bindings substitutions expression

locals :: forall f. Foldable f => f (Tuple (Maybe Ident) Level) -> Array ErlExpr
locals = locals' >>> map S.Var

locals' :: forall f. Foldable f => f (Tuple (Maybe Ident) Level) -> Array String
locals' = Array.fromFoldable >>> mapWithIndex \idx (Tuple name _) ->
  toErlVarWith "Local" name (Level idx)

codegenLetRec ::
  CodegenEnv ->
  { lvl :: Level
  , bindings :: NonEmptyArray (Tuple Ident NeutralExpr)
  } ->
  { bindings :: Array (Tuple String ErlExpr)
  , substitutions :: Map (Tuple (Maybe Ident) Level) ErlExpr
  }
codegenLetRec codegenEnv { lvl, bindings }
  | [ Tuple name directValue ] <- NEA.toArray bindings =
    let
      { vars, value, recName, recCall } = case unwrap directValue of
        Abs vars value ->
          let recName = toErlVar (Just name) lvl in
          { vars: toErlVarExpr <$> NEA.toArray vars
          , value
          , recName
          , recCall:
              if NEA.length vars == 1 then S.Var recName else
              -- We generate an uncurried function, but the callsites expect a curried function still
              -- TODO: inline `S.FunCall _ (S.Fun _ _) _` afterwards
              let recvars = locals vars in
              S.curriedFun recvars (S.FunCall Nothing (S.Var recName) recvars)
          }
        _ ->
          let recName = toErlVarWith "Rec" (Just name) lvl in
          { vars: []
          , value: directValue
          , recName
          , recCall: S.unthunk (S.Var recName)
          }
      renamed = codegenEnv { substitutions = Map.insert (Tuple (Just name) lvl) recCall codegenEnv.substitutions }
    in
      { bindings:
        [ Tuple recName $ S.Fun (Just recName)
          [ Tuple (S.FunHead vars Nothing) $ codegenExpr renamed value ]
        ]
      , substitutions: renamed.substitutions
      }
  | otherwise =
    let
      normal i = toErlVar (Just i) lvl
      local i = toErlVarWith "LocalFn" (Just i) lvl
      mutual i = toErlVarWith "MutualFn" (Just i) lvl

      names = fst <$> NEA.toArray bindings
      bundled scheme = S.Var <<< scheme <$> names

      substitutions = Map.fromFoldable $ names <#>
        \i -> Tuple (Tuple (Just i) lvl) $
          S.FunCall Nothing (S.Var (local i)) (bundled local)
      renamed = codegenEnv { substitutions = substitutions `Map.union` codegenEnv.substitutions }
    in
      { bindings: foldMap (map <<< uncurry)
        [ \i value ->
            Tuple (mutual i) $
              S.simpleFun (bundled local) $
                codegenExpr renamed value
        , \i _ ->
            Tuple (normal i) $
              S.FunCall Nothing (S.Var (mutual i)) (bundled mutual)
        ] $ NEA.toArray bindings
      , substitutions: codegenEnv.substitutions
      }

codegenPrimOp :: CodegenEnv -> BackendOperator NeutralExpr -> ErlExpr
codegenPrimOp codegenEnv = case _ of
  Op1 o x -> do
    let
      x' = codegenExpr codegenEnv x
    case o of
      OpBooleanNot -> S.UnaryOp S.Not x'
      OpIntBitNot -> S.UnaryOp S.BitwiseNot x'
      OpIntNegate -> S.UnaryOp S.Negate x'
      OpNumberNegate -> S.UnaryOp S.Negate x'
      OpArrayLength ->
        S.FunCall (Just $ atomLiteral C.array) (atomLiteral C.size) [ x' ]
      OpIsTag (Qualified _ (Ident constructor)) ->
        S.mIS_TAG (S.atomLiteral (toAtomName constructor)) x'

  Op2 o x y ->
    let
      x' = codegenExpr codegenEnv x
      y' = codegenExpr codegenEnv y

      opOrd :: BackendOperatorOrd -> ErlExpr -> ErlExpr -> ErlExpr
      opOrd =
        case _ of
          OpEq -> S.BinOp S.IdenticalTo
          OpNotEq -> S.BinOp S.NotIdenticalTo
          OpGt -> S.BinOp S.GreaterThan
          OpGte -> S.BinOp S.GreaterThanOrEqualTo
          OpLt -> S.BinOp S.LessThan
          OpLte -> S.BinOp S.LessThanOrEqualTo

    in
      case o of
        OpArrayIndex ->
          S.FunCall (Just $ atomLiteral C.array) (atomLiteral C.get) [ y', x' ]
        OpBooleanAnd ->
          S.BinOp S.AndAlso x' y'
        OpBooleanOr ->
          S.BinOp S.OrElse x' y'
        OpBooleanOrd o' -> opOrd o' x' y'
        OpCharOrd o' -> opOrd o' x' y'
        OpIntBitAnd ->
          S.BinOp S.BitwiseAnd x' y'
        OpIntBitOr ->
          S.BinOp S.BitwiseOr x' y'
        OpIntBitShiftLeft ->
          S.BinOp S.ShiftLeft x' y'

        OpIntBitXor ->
          S.BinOp S.XOr x' y'

        -- Bitshift semantics don't exactly match given erlang integers
        OpIntBitShiftRight ->
          S.BinOp S.ShiftRight x' y'
        OpIntBitZeroFillShiftRight ->
          S.BinOp S.ShiftRight x' y'

        OpIntNum o' -> S.BinOp
          ( case o' of
              OpAdd -> S.Add
              -- FIXME
              OpDivide -> S.IDivide
              OpMultiply -> S.Multiply
              OpSubtract -> S.Subtract
          )
          x'
          y'
        OpIntOrd o' -> opOrd o' x' y'
        OpNumberNum o' -> S.BinOp
          ( case o' of
              OpAdd -> S.Add
              OpDivide -> S.FDivide
              OpMultiply -> S.Multiply
              OpSubtract -> S.Subtract
          )
          x'
          y'
        OpNumberOrd o' -> opOrd o' x' y'
        OpStringAppend ->
          S.BinaryAppend x' y'
        OpStringOrd o' -> opOrd o' x' y'

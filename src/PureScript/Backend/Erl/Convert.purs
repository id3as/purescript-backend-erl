module PureScript.Backend.Erl.Convert where

import Prelude

import Control.Alternative (guard, (<|>))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Foldable (class Foldable, foldMap, foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.Profunctor.Strong ((&&&))
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Erl.Calling (CallPS(..), CallingPS(..), Conventions, Converters, GlobalErl(..), applyConventions, callAs, callAs', callErl, callPS, converts', thunkErl)
import PureScript.Backend.Erl.Constants as C
import PureScript.Backend.Erl.Convert.After (optimizePatterns)
import PureScript.Backend.Erl.Convert.Before (renameRoot)
import PureScript.Backend.Erl.Convert.Common (erlModuleNameForeign, erlModuleNamePs, tagAtom, toAtomName, toErlVar, toErlVarExpr, toErlVarName, toErlVarPat, toErlVarWith)
import PureScript.Backend.Erl.Convert.Foreign (codegenForeign)
import PureScript.Backend.Erl.Parser (ForeignDecls)
import PureScript.Backend.Erl.Syntax (Accessor(..), ErlDefinition(..), ErlExport(..), ErlExpr, ErlModule, ErlPattern, access, atomLiteral, mIS_KNOWN_TAG, mIS_TAG, self)
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
  , callingConventions :: Converters
  , callsHandled :: Boolean
  , constructors :: Map (Qualified Ident) Int
  }

type AcrossModules =
  { callingConventions :: Conventions
  , constructors :: Map (Qualified Ident) Int
  }

initAcrossModules = { callingConventions: mempty, constructors: Map.empty }

codegenModule :: BackendModule -> ForeignDecls -> AcrossModules -> Tuple ErlModule AcrossModules
codegenModule { name: currentModule, bindings, imports, foreign: foreign_, dataTypes, exports: moduleExports } foreigns rolling =
  let
    Tuple thisModuleConventions thisModuleBindings = foldMap (codegenTopLevelBindingGroup currentModule) bindings
    withForeignConventions = rolling.callingConventions <> foldMap fst reexportForeigns
    callingConventions = withForeignConventions <> thisModuleConventions
    -- We do not qualify same-module bindings
    localConventions = thisModuleConventions <#> (map <<< map <<< lmap) (over GlobalErl _ { module = Nothing })
    localCallingConventions = withForeignConventions <> localConventions
    constructors = Map.union rolling.constructors $ Map.fromFoldable $ Array.fromFoldable dataTypes >>= \{ constructors: cTors } ->
      Map.toUnfoldable cTors <#> \(Tuple ctor { fields }) -> Tuple (Qualified (Just currentModule) ctor) (Array.length fields)
    narrowToImports = over SemigroupMap $ Map.filterKeys case _ of
      Qualified (Just mn) _ -> Set.member mn imports || mn == currentModule
      _ -> true
    codegenEnv =
      { currentModule
      , substitutions: Map.empty
      , callingConventions: applyConventions $ narrowToImports localCallingConventions
      , callsHandled: false
      , constructors
      }

    definitions :: Array ErlDefinition
    definitions = Array.concat
      [ thisModuleBindings <@> codegenEnv
      , reexportForeigns <#> snd
      ] <#> \(FunctionDefinition name args expr) ->
        FunctionDefinition name args (optimizePatterns expr)

    reexportForeigns :: Array (Tuple Conventions ErlDefinition)
    reexportForeigns = foreigns.exported >>= \(Tuple decl arity) -> do
      guard $ Ident decl `Set.member` foreign_
      let
        Tuple pats vars = patsAndVars $
          Array.replicate arity unit
            # mapWithIndex \i _ ->
              toErlVar Nothing (Level i)
        calling =
          case NEA.fromArray (void vars) of
            Nothing ->
              callAs' (Qualified (Just currentModule) (Ident decl)) BasePS
                (GlobalErl { module: Just $ erlModuleNameForeign currentModule, name: decl })
                (callErl [])
            Just vars' ->
              callAs' (Qualified (Just currentModule) (Ident decl)) (callPS (Curried vars'))
                (GlobalErl { module: Just $ erlModuleNameForeign currentModule, name: decl })
                (callErl (void vars))
      pure $ Tuple calling $ FunctionDefinition decl [] $
        S.curriedFun pats $
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
      { callingConventions
      , constructors
      }

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
    CtorDef _ _ tag fields | Tuple pats vars <- patsAndVars $ toErlVarName <$> fields -> pure
      [ const $ FunctionDefinition i [] $
          S.curriedFun pats $
            S.Tupled $ [ tagAtom tag ] <> vars
      ]
    Abs vars e ->
      callThisAs (callPS (Curried (void vars))) (callErl (void (NEA.toArray vars)))
      let Tuple epats evars = locals vars in
      [ const $ FunctionDefinition i [] $ S.curriedFun epats $
          S.FunCall Nothing (S.Literal (S.Atom i)) evars
      , \codegenEnv -> FunctionDefinition i (uncurry toErlVar <$> NEA.toArray vars) $ codegenExpr codegenEnv e
      ]
    UncurriedAbs vars e | Array.length vars > 0 ->
      callThisAs (callPS (Uncurried (void vars))) (callErl (void vars))
      let Tuple epats evars = locals vars in
      [ const $ FunctionDefinition i [] $ S.simpleFun epats $
          S.FunCall Nothing (S.Literal (S.Atom i)) evars
      , \codegenEnv -> FunctionDefinition i (uncurry toErlVar <$> vars) $ codegenExpr codegenEnv e
      ]
    UncurriedEffectAbs vars e | Array.length vars > 0 ->
      callThisAs (callPS (UncurriedEffect (void vars))) (callErl (void vars) <|> thunkErl)
      let Tuple epats evars = locals vars in
      [ const $ FunctionDefinition i [] $ S.simpleFun epats $
          S.FunCall Nothing (S.Literal (S.Atom i)) evars
      , \codegenEnv -> FunctionDefinition i (uncurry toErlVar <$> vars) $ codegenChain effectChainMode codegenEnv e
      ]
    _ -> pure
      [ \codegenEnv -> FunctionDefinition i [] $ codegenExpr codegenEnv n ]
  where
  callThisAs x y = Tuple (callAs (Qualified (Just currentModule) (Ident i)) x y)

-- When we recurse in the function side of applications, we don't need to
-- consider calling conventions: they were already handled
codegenExpr0 :: CodegenEnv -> NeutralExpr -> ErlExpr
codegenExpr0 codegenEnv = codegenExpr (setCallsHandled true codegenEnv)

setCallsHandled :: Boolean -> CodegenEnv -> CodegenEnv
setCallsHandled callsHandled codegenEnv | codegenEnv.callsHandled == callsHandled = codegenEnv
setCallsHandled callsHandled codegenEnv = codegenEnv { callsHandled = callsHandled }

codegenExpr :: CodegenEnv -> NeutralExpr -> ErlExpr
codegenExpr codegenEnv0@{ currentModule } s | codegenEnv <- setCallsHandled false codegenEnv0 = case unwrap s of
  _ | not codegenEnv.callsHandled, Just result <- codegenForeign (codegenExpr codegenEnv) s ->
    result

  _ | not codegenEnv.callsHandled, Just result <- converts' codegenEnv.callingConventions (codegenExpr codegenEnv) s ->
    result

  Var (Qualified (Just mn) (Ident i)) | mn /= currentModule ->
    S.FunCall (Just (S.atomLiteral $ erlModuleNamePs mn)) (S.atomLiteral i) []

  Var (Qualified _ (Ident i)) ->
    S.FunCall Nothing (S.atomLiteral i) []

  Local i l | Just replacement <- Map.lookup (Tuple i l) codegenEnv.substitutions ->
    replacement
  Local i l ->
    S.Var (toErlVar i l) self
  Lit l ->
    codegenLiteral codegenEnv l
  App f p ->
    S.curriedApp (codegenExpr0 codegenEnv f) (codegenExpr codegenEnv <$> p)
  Abs a e -> do
    S.curriedFun (toErlVarPat <$> a) (codegenExpr codegenEnv e)
  UncurriedApp f p ->
    S.FunCall Nothing (codegenExpr0 codegenEnv f) (codegenExpr codegenEnv <$> p)
  UncurriedAbs a e ->
    S.simpleFun (toErlVarPat <$> a) (codegenExpr codegenEnv e)
  UncurriedEffectApp f p ->
    S.thunk $ S.FunCall Nothing (codegenExpr0 codegenEnv f) (codegenExpr codegenEnv <$> p)
  UncurriedEffectAbs a e ->
    S.simpleFun (toErlVarPat <$> a) (codegenChain effectChainMode codegenEnv e)

  Accessor e (GetProp i) ->
    access (AcsKey i) (codegenExpr codegenEnv e)

  Accessor e (GetIndex i) ->
    S.FunCall (Just $ atomLiteral C.array) (atomLiteral C.get)
      [ S.numberLiteral i, codegenExpr codegenEnv e ]

  Accessor e (GetCtorField _ _ _ _ _ i) ->
    -- plus 1 for tag at beginning of tuple
    -- plus 1 for 1-indexing
    access (AcsElement (i + 2)) (codegenExpr codegenEnv e)

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
          ( S.CaseClause (S.MatchLiteral (S.Atom C.true_)) Nothing e NEA.:
              NEA.singleton (S.CaseClause S.Discard Nothing ee)
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

locals :: forall f. Foldable f => f (Tuple (Maybe Ident) Level) -> Tuple (Array ErlPattern) (Array ErlExpr)
locals = locals' >>> patsAndVars

patsAndVars :: Array String -> Tuple (Array ErlPattern) (Array ErlExpr)
patsAndVars = map S.bindOrDiscard &&& map (S.Var <@> self)

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
          { vars: toErlVarPat <$> NEA.toArray vars
          , value
          , recName
          , recCall:
              if NEA.length vars == 1 then S.Var recName self else
              -- We generate an uncurried function, but the callsites expect a curried function still
              -- TODO: inline `S.FunCall _ (S.Fun _ _) _` afterwards
              let Tuple recpats recvars = locals vars in
              S.curriedFun recpats (S.FunCall Nothing (S.Var recName self) recvars)
          }
        _ ->
          let recName = toErlVarWith "Rec" (Just name) lvl in
          { vars: []
          , value: directValue
          , recName
          , recCall: S.unthunk (S.Var recName self)
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
      bundledPat scheme = S.BindVar <<< scheme <$> names
      bundled scheme = (S.Var <@> self) <<< scheme <$> names

      substitutions = Map.fromFoldable $ names <#>
        \i -> Tuple (Tuple (Just i) lvl) $
          S.FunCall Nothing (S.Var (local i) self) (bundled local)
      renamed = codegenEnv { substitutions = substitutions `Map.union` codegenEnv.substitutions }
    in
      { bindings: foldMap (map <<< uncurry)
        [ \i value ->
            Tuple (mutual i) $
              S.simpleFun (bundledPat local) $
                codegenExpr renamed value
        , \i _ ->
            Tuple (normal i) $
              S.FunCall Nothing (S.Var (mutual i) self) (bundled mutual)
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
      OpIsTag qi@(Qualified _ (Ident constructor)) | Just arity <- Map.lookup qi codegenEnv.constructors ->
        mIS_KNOWN_TAG (S.atomLiteral (toAtomName constructor)) arity x'
      OpIsTag (Qualified _ (Ident constructor)) ->
        mIS_TAG (S.atomLiteral (toAtomName constructor)) x'
        -- access (AcsTag (toAtomName constructor)) x'

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

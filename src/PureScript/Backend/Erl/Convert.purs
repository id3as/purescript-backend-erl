module PureScript.Backend.Erl.Convert where

import Prelude

import Control.Alternative (guard, (<|>))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (bimap, lmap)
import Data.Foldable (class Foldable, foldMap, foldr)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.Profunctor.Strong ((&&&))
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Erl.Calling (ArityPS, CWB(..), CallPS(..), CallingPS(..), Conventions, Converters, GlobalErl(..), applyCall, applyConventions, arity, callAs, callAs', callErl, callPS, converts', thunkErl, toBase, zipAgainst)
import PureScript.Backend.Erl.Constants as C
import PureScript.Backend.Erl.Convert.After (optimizePatternsDecl)
import PureScript.Backend.Erl.Convert.Common (erlModuleNameForeign, erlModuleNamePs, tagAtom, toAtomName, toErlVar, toErlVarName, toErlVarPat, toErlVarWith)
import PureScript.Backend.Erl.Convert.Foreign (codegenForeign)
import PureScript.Backend.Erl.Foreign.Analyze (Analyzer, analyzeCustom)
import PureScript.Backend.Erl.Parser (ForeignDecls)
import PureScript.Backend.Erl.Syntax (Accessor(..), ErlDefinition(..), ErlExport(..), ErlExpr, ErlModule, ErlPattern, access, atomLiteral, intLiteral, mIS_KNOWN_TAG, mMEMOIZE_AS, self)
import PureScript.Backend.Erl.Syntax as S
import PureScript.Backend.Optimizer.Convert (BackendModule, ConvertEnv, getCtx, makeExternEvalRef, makeExternEvalSpine)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName(..), Prop(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (BackendExpr(..), BackendSemantics(..), Ctx(..), Env(..), LocalBinding(..), NeutralExpr(..), foldBackendExpr, optimize)
import PureScript.Backend.Optimizer.Semantics.Foreign (ForeignEval)
import PureScript.Backend.Optimizer.Syntax (BackendAccessor(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Level(..), Pair(..))

type CodegenEnv =
  { currentModule :: ModuleName
  -- A local identifier can be replaced with an expression. This is used in
  -- the generation of LetRec, where recursive calls need to be modified
  , substitutions :: Map (Tuple (Maybe Ident) Level) ErlExpr
  , callingConventions :: Converters
  , callsHandled :: Boolean
  , constructors :: Map (Qualified Ident) Int
  , custom :: CustomCodegen
  }

type CustomCodegen =
  { customEval :: Map.Map (Qualified Ident) ForeignEval
  , customCodegen :: Converters
  , customAnalysis :: Array Analyzer
  }

type AcrossModules =
  { callingConventions :: Conventions
  , constructors :: Map (Qualified Ident) Int
  }

initAcrossModules :: AcrossModules
initAcrossModules = { callingConventions: mempty, constructors: Map.empty }

data Mode = Debug | NoDebug

codegenModule :: CustomCodegen -> Mode -> BackendModule -> ForeignDecls -> AcrossModules -> Tuple ErlModule AcrossModules
codegenModule custom mode backendModule@{ name: currentModule, bindings, imports, foreign: foreign_, dataTypes } foreigns rolling =
  let
    allBindings = bindings >>= _.bindings
    Tuple thisModuleConventions thisModuleBindings = foldMap (codegenTopLevelBinding currentModule) allBindings

    -- Transitively uncurry some definitions
    moreModuleStuff = uncurryMore custom backendModule (withForeignConventions <> thisModuleConventions) allBindings
    moreModuleBindings = snd moreModuleStuff
    moreModuleConventions = thisModuleConventions <> fst moreModuleStuff

    withForeignConventions = rolling.callingConventions <> foldMap fst reexportForeigns

    -- Calling conventions to export
    callingConventions = withForeignConventions <> moreModuleConventions
    -- Locally we do not qualify same-module bindings
    localConventions = moreModuleConventions <#>
      (map <<< map) do
        bimap
          (lmap (over GlobalErl _ { module = Nothing }))
          (lmap (over GlobalErl _ { module = Nothing }))
    localCallingConventions = localConventions <> withForeignConventions

    -- Constructors to export
    constructors = Map.union rolling.constructors $ Map.fromFoldable $
      Array.fromFoldable dataTypes >>= \{ constructors: cTors } ->
        Map.toUnfoldable cTors <#> \(Tuple ctor { fields }) ->
          Tuple (Qualified (Just currentModule) ctor) (Array.length fields)

    -- Performance boost by filtering the calling conventions to the imports
    narrowToImports = over SemigroupMap $ Map.filterKeys case _ of
      Qualified (Just mn) _ -> Set.member mn imports || mn == currentModule
      _ -> true

    codegenEnv =
      { currentModule
      , substitutions: Map.empty
      , callingConventions: applyConventions $ narrowToImports localCallingConventions
      , callsHandled: false
      , constructors
      , custom
      }

    reexportForeigns :: Array (Tuple Conventions ErlDefinition)
    reexportForeigns = foreigns.exported >>= \(Tuple decl arity) -> do
      guard $ Ident decl `Set.member` foreign_
      let
        Tuple pats vars = patsAndVars $ autoVars $ Array.replicate arity unit
        calling =
          case NEA.fromArray (void vars) of
            -- Foreign imports with arity 0 are taken as-is
            Nothing ->
              callAs' (Qualified (Just currentModule) (Ident decl)) BasePS
                (GlobalErl { module: Just $ erlModuleNameForeign currentModule, name: decl })
                (callErl [])
            -- Foreign imports with nonzero arity get curried by that amount
            Just vars' ->
              callAs' (Qualified (Just currentModule) (Ident decl)) (callPS (Curried vars'))
                (GlobalErl { module: Just $ erlModuleNameForeign currentModule, name: decl })
                (callErl (void vars))
      pure $ Tuple calling $ FunctionDefinition decl [] $
        S.curriedFun pats $
          S.FunCall (Just (S.atomLiteral (erlModuleNameForeign currentModule))) (S.atomLiteral decl) vars

    { rawDefinitions, definitions } = case mode of
      Debug ->
        let
          rawDefinitions = Array.concat
            [ thisModuleBindings <@> codegenEnv
            , reexportForeigns <#> snd
            , moreModuleBindings <@> codegenEnv
            ]
          definitions = optimizePatternsDecl <$> rawDefinitions
        in { rawDefinitions, definitions }
      NoDebug ->
        let
          definitions = Array.concat
            [ thisModuleBindings <@> codegenEnv
            , reexportForeigns <#> snd
            , moreModuleBindings <@> codegenEnv
            ] <#> optimizePatternsDecl
        in { rawDefinitions: definitions, definitions }

    exports :: Array ErlExport
    exports = definitionExports <$> definitions

  in
    Tuple
      { moduleName: erlModuleNamePs currentModule
      , definitions
      , rawDefinitions
      , exports
      , comments: [ unwrap currentModule ]
      }
      { callingConventions
      , constructors
      }

definitionExports :: ErlDefinition -> ErlExport
definitionExports = case _ of
  FunctionDefinition f a _ ->
    Export f (Array.length a)

reOptimize :: CustomCodegen -> BackendModule -> Qualified Ident -> Array (Tuple (Maybe Ident) Level) -> NeutralExpr -> NeutralExpr
reOptimize custom thisModule qualifiedIdent addedLocals neutralExpr =
  foldBackendExpr NeutralExpr (\_ neutExpr -> neutExpr) $ snd $
    optimize false ctx evalEnv qualifiedIdent 100 backendExpr
  where
  env = (convertEnv custom thisModule)
    { currentLevel = Array.length addedLocals
    , toLevel = Map.fromFoldable $ addedLocals # Array.mapMaybe \(Tuple mi l) -> Tuple <$> mi <@> l
    }
  ctx = getCtx env
  Ctx { analyze } = ctx
  toBackendExpr (NeutralExpr syntax) =
    let syntax' = syntax <#> toBackendExpr
    in ExprSyntax (analyze ctx syntax') syntax'
  backendExpr = toBackendExpr neutralExpr
  evalEnv = Env
    { currentModule: env.currentModule
    , evalExternRef: makeExternEvalRef env
    , evalExternSpine: makeExternEvalSpine env
    , locals: addedLocals <#> One <<< uncurry NeutLocal
    , directives: env.directives
    }

shift :: Int -> NeutralExpr -> NeutralExpr
shift amt (NeutralExpr (Local i (Level l))) = NeutralExpr (Local i (Level (l + amt)))
shift amt (NeutralExpr (Abs ils a)) = NeutralExpr (Abs (map (map \(Level l) -> Level (l + amt)) ils) (shift amt a))
shift amt (NeutralExpr (UncurriedAbs ils a)) = NeutralExpr (UncurriedAbs (map (map \(Level l) -> Level (l + amt)) ils) (shift amt a))
shift amt (NeutralExpr (UncurriedEffectAbs ils a)) = NeutralExpr (UncurriedEffectAbs (map (map \(Level l) -> Level (l + amt)) ils) (shift amt a))
shift amt (NeutralExpr (LetRec (Level l) as a)) = NeutralExpr (LetRec (Level (l + amt)) (map (shift amt) <$> as) (shift amt a))
shift amt (NeutralExpr (Let i (Level l) a1 a2)) = NeutralExpr (Let i (Level (l + amt)) (shift amt a1) (shift amt a2))
shift amt (NeutralExpr (EffectBind i (Level l) a1 a2)) = NeutralExpr (EffectBind i (Level (l + amt)) (shift amt a1) (shift amt a2))
shift amt (NeutralExpr x) = NeutralExpr (map (shift amt) x)

convertEnv :: CustomCodegen -> BackendModule -> ConvertEnv
convertEnv { customAnalysis, customEval } { name, implementations, directives } =
  { analyzeCustom: analyzeCustom customAnalysis
  , currentModule: name
  , currentLevel: 0
  , toLevel: Map.empty
  , implementations
  , moduleImplementations: Map.empty
  , directives
  , dataTypes: Map.empty
  , foreignSemantics: customEval
  , rewriteLimit: 10_000
  , traceIdents: mempty
  , optimizationSteps: []
  }

uncurryMore ::
  CustomCodegen ->
  BackendModule ->
  Conventions ->
  Array (Tuple Ident NeutralExpr) ->
  Tuple Conventions (Array (CodegenEnv -> ErlDefinition))
uncurryMore custom mod c0 decls = go mempty []
  where
  go c1 added =
    case foldMap (uncurryMore1 custom mod (c0 <> c1)) decls of
      Tuple _ [] -> Tuple c1 added
      Tuple c2 more ->
        go (c1 <> c2) (added <> more)

uncurryMore1 ::
  CustomCodegen ->
  BackendModule ->
  Conventions ->
  Tuple Ident NeutralExpr ->
  Tuple Conventions (Array (CodegenEnv -> ErlDefinition))
uncurryMore1 custom backendModule (SemigroupMap conventions) (Tuple (Ident i) n) =
  -- Dredge up some partially applied definitions
  case toBase unit n of
    Just (CWB qi args) | Just arities <- Map.lookup qi conventions ->
      arities # foldMapWithIndex case _, _ of
        psCall, _erlConvention
          | arity psCall > arity args -- this is not redundant with the next line
          , Just { matched, unmatched: Just unmatched } <- zipAgainst psCall args
          , isNewOptimization thisBinding unmatched (SemigroupMap conventions) ->
            -- abstract out the unmatched variables
            let Tuple abstracted localVars = psPatsAndVars $ autoPSVars unmatched in
            -- the Erlang definition will be called with them all flat
            let abstractedFlat = toErlVarPat <$> Array.fromFoldable abstracted in
            -- Record the optimization
            callThisAs unmatched (void $ callErl abstractedFlat) $
              Array.singleton $
                \codegenEnv -> FunctionDefinition i abstractedFlat $ codegenExpr codegenEnv $
                  -- catch some optimizations, like `+` inlining and MagicDo
                  reOptimize custom backendModule thisBinding (Array.fromFoldable abstracted) $
                    -- stitch together the new call with the extra variables,
                    -- which will trigger specialization in `codegenExpr` if
                    -- it isn't already optimized by the backend-optimizer
                    applyCall unit qi $
                      shift (arity localVars) <$> args <|> localVars
        _, _ -> mempty
    _ -> mempty
  where
  thisBinding = Qualified (Just backendModule.name) (Ident i)
  callThisAs x y = Tuple (callAs thisBinding x y)

isNewOptimization :: Qualified Ident -> ArityPS -> Conventions -> Boolean
isNewOptimization qi callingPS (SemigroupMap conventions) =
  case Map.lookup qi conventions of
    Nothing -> true
    Just (SemigroupMap arities) ->
      not Map.member callingPS arities

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
      [ const $ FunctionDefinition i [] $ case NEA.length vars of
          1 -> S.FunName Nothing (S.Literal (S.Atom i)) 1
          _ -> S.curriedFun epats $
            S.FunCall Nothing (S.Literal (S.Atom i)) evars
      , \codegenEnv -> FunctionDefinition i (toErlVarPat <$> NEA.toArray vars) $ codegenExpr codegenEnv e
      ]
    UncurriedAbs vars e | Array.length vars > 0 ->
      callThisAs (callPS (Uncurried (void vars))) (callErl (void vars))
      let Tuple epats evars = locals vars in
      [ const $ FunctionDefinition i [] $ S.simpleFun epats $
          S.FunCall Nothing (S.Literal (S.Atom i)) evars
      , \codegenEnv -> FunctionDefinition i (toErlVarPat <$> vars) $ codegenExpr codegenEnv e
      ]
    UncurriedEffectAbs vars e | Array.length vars > 0 ->
      callThisAs (callPS (UncurriedEffect (void vars))) (callErl (void vars) <|> thunkErl)
      let Tuple epats evars = locals vars in
      [ const $ FunctionDefinition i [] $ S.simpleFun epats $
          S.FunCall Nothing (S.Literal (S.Atom i)) evars
      , \codegenEnv -> FunctionDefinition i (toErlVarPat <$> vars) $ codegenChain effectChainMode codegenEnv e
      ]
    _ -> pure
      [ \codegenEnv -> FunctionDefinition i [] $ tryMemoize $ codegenExpr codegenEnv n ]
  where
  callThisAs x y = Tuple (callAs (Qualified (Just currentModule) (Ident i)) x y)
  tryMemoize expr
    -- Do not memoize lambdas
    | S.Fun _ _ <- expr = expr
    -- Preserve guard expressions
    | S.guardExpr expr = expr
    -- Do not memoize small terms
    | otherwise =
      case S.estimatedComplexity expr of
        c | c < 16 -> expr
        c -> mMEMOIZE_AS [ erlModuleNamePs currentModule, i ] (intLiteral c) expr

-- When we recurse in the function side of applications, we don't need to
-- consider calling conventions: they were already handled
codegenExpr0 :: CodegenEnv -> NeutralExpr -> ErlExpr
codegenExpr0 codegenEnv = codegenExpr (setCallsHandled true codegenEnv)

setCallsHandled :: Boolean -> CodegenEnv -> CodegenEnv
setCallsHandled callsHandled codegenEnv | codegenEnv.callsHandled == callsHandled = codegenEnv
setCallsHandled callsHandled codegenEnv = codegenEnv { callsHandled = callsHandled }

codegenExpr :: CodegenEnv -> NeutralExpr -> ErlExpr
codegenExpr codegenEnv0@{ currentModule } s | codegenEnv <- setCallsHandled false codegenEnv0 = case unwrap s of
  _ | not codegenEnv.callsHandled, Just result <- codegenForeign codegenEnv.custom.customCodegen (codegenExpr codegenEnv) s ->
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
    S.RecordUpdate (codegenExpr codegenEnv e) ((\(Prop name p) -> Tuple name (codegenExpr codegenEnv p)) <$> props)

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
        S.If $ S.IfClause (S.Guard c) e NEA.: case ee of
          S.If clauses ->
            clauses
          _ ->
            NEA.singleton (S.IfClause (S.Guard $ S.Literal $ S.Atom "true") ee)
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
    S.FunCall (Just $ atomLiteral C.erlang) (atomLiteral C.error) [
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
  LitRecord r -> S.Record $ (\(Prop f e) -> Tuple f (codegenExpr codegenEnv e)) <$> r

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

autoVars :: forall a. Array a -> Array String
autoVars = mapWithIndex \i _ ->
  toErlVar Nothing (Level i)

locals' :: forall f. Foldable f => f (Tuple (Maybe Ident) Level) -> Array String
locals' = Array.fromFoldable >>> mapWithIndex \idx (Tuple name _) ->
  toErlVarWith "Local" name (Level idx)

psPatsAndVars :: forall f. Functor f => f (Tuple (Maybe Ident) Level) -> Tuple (f (Tuple (Maybe Ident) Level)) (f NeutralExpr)
psPatsAndVars = identity &&& map (NeutralExpr <<< uncurry Local)

autoPSVars :: forall f a. FunctorWithIndex Int f => f a -> f (Tuple (Maybe Ident) Level)
autoPSVars = mapWithIndex \i _ ->
  Tuple Nothing (Level i)

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
      OpIsTag (Qualified mn (Ident constructor)) ->
        unsafeCrashWith $ "Unknown constructor: " <> foldMap (\(ModuleName n) -> n <> ".") mn <> constructor

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

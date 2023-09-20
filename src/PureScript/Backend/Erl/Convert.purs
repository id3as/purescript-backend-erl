module PureScript.Backend.Erl.Convert where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.CodePoint.Unicode as StringCP
import Data.Foldable (foldMap, foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', maybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Erl.Constants as C
import PureScript.Backend.Erl.Parser (ForeignDecls)
import PureScript.Backend.Erl.Syntax (ErlDefinition(..), ErlExport(..), ErlExpr, ErlModule, atomLiteral)
import PureScript.Backend.Erl.Syntax as S
import PureScript.Backend.Optimizer.Convert (BackendModule, BackendBindingGroup)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName(..), Prop(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (NeutralExpr(..))
import PureScript.Backend.Optimizer.Syntax (BackendAccessor(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Level(..), Pair(..))

type CodegenEnv =
  { currentModule :: ModuleName
  -- A local identifier can be replaced with an expression. This is used in
  -- the generation of LetRec, where recursive calls need to be modified
  , substitutions :: Map (Tuple (Maybe Ident) Level) ErlExpr
  }

codegenModule :: BackendModule -> ForeignDecls -> ErlModule
codegenModule { name, bindings, imports, foreign: foreign_ } foreigns =
  let
    codegenEnv :: CodegenEnv
    codegenEnv = { currentModule: name, substitutions: Map.empty }

    definitions :: Array ErlDefinition
    definitions = Array.concat
      [ codegenTopLevelBindingGroup codegenEnv =<< bindings
      , reexports
      ]

    reexports :: Array ErlDefinition
    reexports = foreigns.exported <#> \(Tuple decl arity) -> do
      let
        vars =
          Array.replicate arity unit
            # mapWithIndex \i _ ->
              toErlVarExpr (Tuple Nothing (Level i))
      -- Uhh this is definitely wrong, at least in some cases
      -- (EffectFn?)
      FunctionDefinition decl [] $
        S.curriedFun vars $
          S.FunCall (Just (S.atomLiteral (erlModuleNameForeign name))) (S.atomLiteral decl) vars

    exports :: Array ErlExport
    exports = Array.concatMap definitionExports definitions

  in
    { moduleName: erlModuleNamePs name
    , definitions
    , exports
    }

definitionExports :: ErlDefinition -> Array ErlExport
definitionExports = case _ of
  FunctionDefinition f a _ -> [ Export f (Array.length a) ]
  _ -> []

erlModuleNameCommon :: ModuleName -> String
erlModuleNameCommon name =
  String.joinWith "_"
    $ map toAtomName
    $ String.split (String.Pattern ".") (unwrap name)

erlModuleNamePs :: ModuleName -> String
erlModuleNamePs name = erlModuleNameCommon name <> "@ps"

erlModuleNameForeign :: ModuleName -> String
erlModuleNameForeign name = erlModuleNameCommon name <> "@foreign"

toAtomName :: String -> String
toAtomName text = case String.uncons text of
  Just { head, tail } -> String.fromCodePointArray (StringCP.toLower head) <> tail
  Nothing -> text

toErlVar :: Maybe Ident -> Level -> String
toErlVar text (Level lvl) =
  maybe "V" (toErlVarName <<< unwrap) text <> "@" <> show lvl

toErlVarWith :: String -> Maybe Ident -> Level -> String
toErlVarWith suffix text (Level lvl) =
  maybe "V" (toErlVarName <<< unwrap) text <> "@" <> suffix <> "@" <> show lvl

-- String.replace (String.Pattern ".") (String.Replacement "_") (unwrap name)
--   T.intercalate "_" (toAtomName <$> T.splitOn "." name)

toErlVarName :: String -> String
toErlVarName text = case String.uncons text of
  Just { head, tail } ->
    -- Lazy to include start
    String.replace (String.Pattern "'") (String.Replacement "_@prime") $
    String.replace (String.Pattern "$") (String.Replacement "_@dollar") $

     String.fromCodePointArray (StringCP.toUpper head) <> tail
  Nothing -> "V"

toErlVarExpr :: Tuple (Maybe Ident) Level -> ErlExpr
toErlVarExpr = S.Var <<< uncurry toErlVar

tagAtom :: Ident -> ErlExpr
tagAtom tagName = S.Literal $ S.Atom $ toAtomName $ unwrap tagName

codegenTopLevelBindingGroup
  :: CodegenEnv
  -> BackendBindingGroup Ident NeutralExpr
  -> Array ErlDefinition
codegenTopLevelBindingGroup codegenEnv { bindings } =
  Array.concatMap (codegenTopLevelBinding codegenEnv) bindings

codegenTopLevelBinding
  :: CodegenEnv
  -> Tuple Ident NeutralExpr
  -> Array ErlDefinition
codegenTopLevelBinding codegenEnv (Tuple (Ident i) n) =
  case unwrap n of
    CtorDef _ _ tag fields | vars <- S.Var <<< toErlVarName <$> fields ->
      [ FunctionDefinition i [] $
          S.curriedFun vars $
            S.Tupled $ [ tagAtom tag ] <> vars
      ]
    _ ->
      [ FunctionDefinition i [] $ codegenExpr codegenEnv n ]

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

codegenExpr :: CodegenEnv -> NeutralExpr -> ErlExpr
codegenExpr codegenEnv@{ currentModule } s = case unwrap s of
  _ | Just result <- codegenList codegenEnv s ->
    result

  _ | Just [NeutralExpr (Lit (LitString value))] <- helper "Erl.Atom" "atom" 1 s ->
    S.Literal (S.Atom value)

  _ | Just [] <- helper "Data.Unit" "unit" 0 s ->
    S.Literal (S.Atom "unit")

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
    S.curriedApp (codegenExpr codegenEnv f) (codegenExpr codegenEnv <$> p)
  Abs a e -> do
    S.curriedFun (toErlVarExpr <$> a) (codegenExpr codegenEnv e)
  UncurriedApp f p ->
    S.FunCall Nothing (codegenExpr codegenEnv f) (codegenExpr codegenEnv <$> p)
  UncurriedAbs a e ->
    S.simpleFun (toErlVarExpr <$> a) (codegenExpr codegenEnv e)
  UncurriedEffectApp f p ->
    S.thunk $ S.FunCall Nothing (codegenExpr codegenEnv f) (codegenExpr codegenEnv <$> p)
  UncurriedEffectAbs a e ->
    S.simpleFun (toErlVarExpr <$> a) (codegenChain effectChainMode codegenEnv e)
  Accessor e (GetProp i) ->
    S.FunCall (Just $ atomLiteral C.maps) (atomLiteral C.get)
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

      go (Tuple c e) ee =
        -- S.If (S.IfClause c Nothing e NEA.: NEA.singleton (S.IfClause (S.Literal $ S.Atom "true") Nothing ee))
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

-- jsonToErlString :: String -> String
-- jsonToErlString str = unicodeReplace str
--   where
--   unicodeRegex :: R.Regex
--   unicodeRegex = R.Unsafe.unsafeRegex """\\u([A-F\d]{4})""" R.Flags.global

--   unicodeReplace :: String -> String
--   unicodeReplace s = R.replace' unicodeRegex unicodeReplaceMatch s

--   unicodeReplaceMatch
--     :: String
--     -> Array (Maybe String)
--     -> String
--   unicodeReplaceMatch _ = case _ of
--     [ (Just x) ] -> "\\x" <> x <> ";"
--     _ -> unsafeCrashWith "Error matching at unicodeReplaceMatch in jsonToErlString"

-- -- > In addition to the standard named characters
-- -- > #\alarm, #\backspace, #\delete, #\esc, #\linefeed, #\newline, #\page, #\return, #\space, and #\tab,
-- -- > Erl Scheme recognizes #\bel, #\ls, #\nel, #\nul, #\rubout, and #\vt (or #\vtab).
-- --
-- -- Source: https://cisco.github.io/ErlScheme/csug9.5/intro.html#./intro:h1, 6th paragraph
-- codegenChar :: Char -> ErlExpr
-- codegenChar c = S.Char $ append """#\""" $ escapeChar $ toCharCode c
--   where
--   escapeChar code
--     | code == toCharCode '\x0000' = "nul"
--     | code == toCharCode '\x0007' = "alarm" -- bel
--     | code == toCharCode '\x0008' = "backspace"
--     | code == toCharCode '\t' = "tab"
--     | code == toCharCode '\n' = "linefeed" -- nel/newline; per R6Rs, newline is deprecated
--     | code == toCharCode '\x000B' = "vtab"
--     | code == toCharCode '\x000C' = "page"
--     | code == toCharCode '\r' = "return"
--     | code == toCharCode '\x001B' = "esc"
--     | code == toCharCode ' ' = "space"
--     | code == toCharCode '\x007F' = "delete" -- rubout
--     | code == toCharCode '\x2028' = "ls"
--     | code < 20 || code > 127 = "x" <> (padLeft "0" 4 $ Int.toStringAs Int.hexadecimal code)
--     | otherwise = CodeUnits.singleton c

--   padLeft char i s = power char (i - String.length s) <> s

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
      maybeUnthunk = if shouldUnthunk then S.unthunk else identity
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
          , recCall: S.Var recName
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
      local i = toErlVarWith "Local" (Just i) lvl
      mutual i = toErlVarWith "Mutual" (Just i) lvl

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

codegenList :: CodegenEnv -> NeutralExpr -> Maybe ErlExpr
codegenList codegenEnv = gather [] <@> finish where
  typesMod = "Erl.Data.List.Types"
  listPrim = helper typesMod
  gen = codegenExpr codegenEnv

  finish = { lit: finishLit, cons: finishCons }
  finishLit acc =
    Just (S.List (gen <$> acc))
  finishCons [] _ =
    Nothing
  finishCons acc s' =
    Just (finishCons' acc s')
  finishCons' [] s' = gen s'
  finishCons' acc s' =
    S.ListCons (gen <$> acc) (gen s')

  gather acc s end = case unit of
    _ | Just [head, tail] <- listPrim "cons" 2 s ->
      gather (acc <> [head]) tail end
    -- _ | Just [l, r] <- listPrim "appendImpl" 2 s ->
    --   gather acc l
    --     { lit: \acc' -> gather acc' r finish
    --     , cons: \acc' l' ->
    --         Just (S.BinOp S.ListConcat (finishCons' acc' l') $ fromMaybe' (\_ -> gen r) (gather [] r finish))
    --     }
    -- TODO: use NeutStop to choose a different normal form for this?
    _ | Just [cons, nil, ls] <- helper "Data.Foldable" "foldrArray" 3 s
      , Just [] <- listPrim "cons" 0 cons
      , Just [] <- listPrim "nil" 0 nil
      , Lit (LitArray acc') <- unwrap ls ->
      end.lit (acc <> acc')
    _ | Just [] <- listPrim "nil" 0 s ->
      end.lit acc
    _ ->
      end.cons acc s


codegenPrimOp :: CodegenEnv -> BackendOperator NeutralExpr -> ErlExpr
codegenPrimOp codegenEnv@{ currentModule } = case _ of
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
        S.BinOp S.IdenticalTo (S.atomLiteral (toAtomName constructor)) $
          S.FunCall (Just $ atomLiteral C.erlang) (atomLiteral C.element)
            [ S.numberLiteral 1, x' ]

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
          S.FunCall (Just $ atomLiteral C.array) (atomLiteral C.get) [ x', y' ]
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
          S.FunCall (Just $ atomLiteral $ C.unicode) (atomLiteral $ C.characters_to_binary)
            [ S.List [ x', y' ]
            , S.atomLiteral C.utf8
            ]
        OpStringOrd o' -> opOrd o' x' y'

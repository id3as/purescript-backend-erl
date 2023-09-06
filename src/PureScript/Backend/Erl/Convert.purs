module PureScript.Backend.Erl.Convert where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.CodePoint.Unicode as StringCP
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String as String
import Data.Tuple (Tuple(..), uncurry)
import PureScript.Backend.Erl.Constants as C
import PureScript.Backend.Erl.Syntax (ErlDefinition(..), ErlExport(..), ErlExpr, ErlModule, atomLiteral)
import PureScript.Backend.Erl.Syntax as S
import PureScript.Backend.Optimizer.Convert (BackendModule, BackendBindingGroup)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName, Prop(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (NeutralExpr)
import PureScript.Backend.Optimizer.Syntax (BackendAccessor(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Level(..), Pair(..))

type CodegenEnv =
  { currentModule :: ModuleName
  }

codegenModule :: BackendModule -> ErlModule
codegenModule { name, bindings, imports, foreign: foreign_ } =
  let
    codegenEnv :: CodegenEnv
    codegenEnv = { currentModule: name }

    definitions :: Array ErlDefinition
    definitions = Array.concat $
      codegenTopLevelBindingGroup codegenEnv <$> bindings

    -- TODO and foreigns
    exports :: Array ErlExport
    exports = Array.concatMap definitionExports definitions

  in
    { moduleName: erlModuleName name
    , definitions
    , exports
    }

definitionExports :: ErlDefinition -> Array ErlExport
definitionExports = case _ of
  FunctionDefinition f a _ -> [ Export f (Array.length a) ]
  _ -> []

erlModuleName name =
  String.joinWith "_"
    $ map toAtomName
    $ String.split (String.Pattern ".") (unwrap name)

toAtomName :: String -> String
toAtomName text = case String.uncons text of
  Just { head, tail } -> String.fromCodePointArray (StringCP.toLower head) <> tail
  Nothing -> text

toErlVar :: Maybe Ident -> Level -> String
toErlVar text (Level lvl) = case text >>= (unwrap >>> String.uncons) of
  Just { head, tail } -> String.fromCodePointArray (StringCP.toUpper head) <> tail <> "@" <> show
    lvl
  Nothing -> "V" <> "@" <> show lvl

-- String.replace (String.Pattern ".") (String.Replacement "_") (unwrap name)
--   T.intercalate "_" (toAtomName <$> T.splitOn "." name) 

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
    CtorDef _ _ _ ss -> [ UnimplementedDefinition ]
    -- CtorDef _ _ _ ss ->
    --   case NonEmptyArray.fromArray ss of
    --     Nothing ->
    --       [ 
    --         Define i (S.quote $ S.Identifier i)
    --       , Define (S.recordTypePredicate i)
    --           $ S.mkUncurriedFn [ "v" ]
    --           $ S.eqQ (S.quote $ S.Identifier i) (S.Identifier "v")
    --       ]
    --     Just xs
    --       | NEA.length xs == 1 ->
    --           [ DefineRecordType i ss ]
    --       | otherwise ->
    --           [ DefineRecordType i ss
    --           , Define i $ S.mkCurriedFn xs
    --               $ S.runUncurriedFn
    --                   (S.Identifier $ S.recordTypeUncurriedConstructor i)
    --                   (map S.Identifier ss)
    --           ]
    _ ->
      [ FunctionDefinition i [] $ codegenExpr codegenEnv n ]

-- [ Define i $ codegenExpr codegenEnv n ]

codegenExpr :: CodegenEnv -> NeutralExpr -> ErlExpr
codegenExpr codegenEnv@{ currentModule } s = case unwrap s of
  Var (Qualified (Just mn) (Ident i)) ->
    S.FunCall (Just (S.atomLiteral $ erlModuleName mn)) (S.atomLiteral i) []

  Var (Qualified (Nothing) (Ident i)) ->
    S.Var i

  Local i l ->
    S.Var $ toErlVar i l
  Lit l ->
    codegenLiteral codegenEnv l
  App f p ->
    S.curriedApp (codegenExpr codegenEnv f) (codegenExpr codegenEnv <$> p)
  Abs a e -> do
    S.curriedFun ((S.Var <<< uncurry toErlVar) <$> a) (codegenExpr codegenEnv e)
  UncurriedApp f p ->
    S.FunCall Nothing (codegenExpr codegenEnv f) (codegenExpr codegenEnv <$> p)
  UncurriedAbs a e ->
    S.Fun Nothing
      [ Tuple
          (S.FunHead ((S.Var <<< uncurry toErlVar) <$> a) Nothing)
          (codegenExpr codegenEnv e)
      ]
  UncurriedEffectApp f p ->
    S.Unimplemented "uncurried_effect_app"
  --     S.thunk $ S.runUncurriedFn (codegenExpr codegenEnv f)
  --       (codegenExpr codegenEnv <$> p)
  UncurriedEffectAbs a e ->
    S.Unimplemented "uncurried_effect_abs"
  --     S.mkUncurriedFn (uncurry toErlIdent <$> a)
  --       (codegenChain effectChainMode codegenEnv e)

  Accessor e (GetProp i) ->
    S.FunCall (Just $ atomLiteral "maps") (atomLiteral "get")
      [ S.atomLiteral i, codegenExpr codegenEnv e ]

  Accessor e (GetIndex i) ->
    S.FunCall (Just $ atomLiteral "array") (atomLiteral "get")
      [ S.numberLiteral i, codegenExpr codegenEnv e ]
  --     S.runUncurriedFn
  --       (S.Identifier $ scmPrefixed "vector-ref")
  --       [ codegenExpr codegenEnv e, S.Integer $ wrap $ show i ]
  Accessor e (GetCtorField qi _ _ _ field _) ->
    S.Unimplemented "GetField"
  --     S.recordAccessor (codegenExpr codegenEnv e) (flattenQualified currentModule qi) field
  Update _ _ ->
    S.Unimplemented "update"
  --     S.Identifier "object-update"

  CtorSaturated qi _ _ _ xs ->
    S.Unimplemented "ctor_saturated"
  --   CtorSaturated qi _ _ _ xs ->
  --     S.runUncurriedFn
  --       (S.Identifier $ S.recordTypeUncurriedConstructor $ flattenQualified currentModule qi)
  --       (map (codegenExpr codegenEnv <<< Tuple.snd) xs)
  CtorDef _ _ _ _ ->
    S.Unimplemented "ctor_def"
  --     unsafeCrashWith "codegenExpr:CtorDef - handled by codegenTopLevelBinding!"
  LetRec lvl bindings expr ->
    S.Unimplemented "letrec"
  --     S.Let true (map (bimap (flip toErlIdent lvl <<< Just) (codegenExpr codegenEnv)) bindings) $
  --       codegenExpr codegenEnv expr
  Let i lvl e e' ->
    S.Block
      [ S.Match (S.Var $ toErlVar i lvl) (codegenExpr codegenEnv e)
      , (codegenExpr codegenEnv e')
      ]

  Branch b o -> do 
    let
      goPair :: Pair NeutralExpr -> Tuple ErlExpr ErlExpr
      goPair (Pair c e) = Tuple (codegenExpr codegenEnv c) (codegenExpr codegenEnv e)
      
      go (Tuple c e) ee =
        -- S.If (S.IfClause c Nothing e NEA.: NEA.singleton (S.IfClause (S.Literal $ S.Atom "true") Nothing ee))
        S.Case c (
          S.CaseClause (S.atomLiteral "true") Nothing e NEA.:
          NEA.singleton (S.CaseClause (S.atomLiteral "_") Nothing ee)
        )

    foldr go (codegenExpr codegenEnv o) (goPair <$> b)
  --     S.Cond (goPair <$> b) (codegenExpr codegenEnv o)

  EffectBind _ _ _ _ -> S.Unimplemented "effect_bind"
  --     codegenEffectChain codegenEnv s
  EffectPure _ ->S.Unimplemented "effect_pure"
  --     codegenEffectChain codegenEnv s
  EffectDefer _ ->S.Unimplemented "effect_defer"
  --     codegenEffectChain codegenEnv s
  PrimEffect _ ->S.Unimplemented "prim_effect"
  --     codegenEffectChain codegenEnv s

  PrimOp o ->
    codegenPrimOp codegenEnv o
  PrimUndefined ->
    S.atomLiteral C.undefined
  --     S.app (S.Identifier $ scmPrefixed "gensym")
  --       (S.StringExpr $ Json.stringify $ Json.fromString undefinedSymbol)

  Fail i -> S.Unimplemented "fail"
  --     -- Note: This can be improved by using `error`, but it requires
  --     -- that we track where exactly this `Fail` is defined. We can
  --     -- make use of the `codegenEnv` for this.
  --     S.List
  --       [ S.Identifier $ scmPrefixed "raise"
  --       , S.List
  --           [ S.Identifier $ scmPrefixed "condition"
  --           , S.List [ S.Identifier $ scmPrefixed "make-error" ]
  --           , S.List
  --               [ S.Identifier $ scmPrefixed "make-message-condition"
  --               , S.StringExpr $ Json.stringify $ Json.fromString i
  --               ]
  --           ]
  --       ]

codegenLiteral :: CodegenEnv -> Literal NeutralExpr -> ErlExpr
codegenLiteral codegenEnv = case _ of
  LitInt i -> S.Literal $ S.Integer i
  LitNumber n -> S.Literal $ S.Float n
  LitString s -> S.Literal $ S.String s
  LitChar c -> S.Literal $ S.Char c
  LitBoolean b -> S.Literal $ S.Atom $ if b then "true" else "false"
  -- TODO arrays are not lists
  LitArray a -> S.List $ codegenExpr codegenEnv <$> a
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

-- type ChainMode = { effect :: Boolean }

-- pureChainMode :: ChainMode
-- pureChainMode = { effect: false }

-- effectChainMode :: ChainMode
-- effectChainMode = { effect: true }

-- codegenPureChain :: CodegenEnv -> NeutralExpr -> ErlExpr
-- codegenPureChain codegenEnv = codegenChain pureChainMode codegenEnv

-- codegenEffectChain :: CodegenEnv -> NeutralExpr -> ErlExpr
-- codegenEffectChain codegenEnv = S.thunk <<< codegenChain effectChainMode codegenEnv

-- codegenChain :: ChainMode -> CodegenEnv -> NeutralExpr -> ErlExpr
-- codegenChain chainMode codegenEnv = collect []
--   where
--   recursive :: Boolean
--   recursive = false

--   -- `expression` has type `Effect ..`, so we can confidently unthunk here
--   codegenEffectBind :: NeutralExpr -> ErlExpr
--   codegenEffectBind expression = case unwrap expression of
--     PrimEffect e' ->
--       codegenPrimEffect codegenEnv e'
--     UncurriedEffectApp f p ->
--       S.runUncurriedFn (codegenExpr codegenEnv f) (codegenExpr codegenEnv <$> p)
--     _ ->
--       S.unthunk $ codegenExpr codegenEnv expression

--   finish :: Boolean -> Array _ -> NeutralExpr -> ErlExpr
--   finish shouldUnthunk bindings expression = do
--     let
--       maybeUnthunk :: ErlExpr -> ErlExpr
--       maybeUnthunk = if shouldUnthunk then S.unthunk else identity
--     case NEA.fromArray bindings of
--       Nothing -> maybeUnthunk $ codegenExpr codegenEnv expression
--       Just bindings' -> S.Let recursive bindings' $ maybeUnthunk $ codegenExpr codegenEnv expression

--   collect :: Array _ -> NeutralExpr -> ErlExpr
--   collect bindings expression = case unwrap expression of
--     Let i l v e' ->
--       collect (Array.snoc bindings $ Tuple (toErlIdent i l) (codegenExpr codegenEnv v)) e'
--     EffectPure e' | chainMode.effect ->
--       finish false bindings e'
--     PrimEffect e' | chainMode.effect ->
--       codegenPrimEffect codegenEnv e'
--     EffectBind i l v e' | chainMode.effect ->
--       collect
--         (Array.snoc bindings $ Tuple (toErlIdent i l) (codegenEffectBind v))
--         e'
--     EffectDefer e' | chainMode.effect ->
--       collect bindings e'
--     _ ->
--       finish chainMode.effect bindings expression

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
      OpArrayLength -> S.FunCall (Just $ atomLiteral "array") (atomLiteral "length") [ x' ]
      OpIsTag _ -> S.Unimplemented "istag"
  --       OpArrayLength ->
  --         S.List [ S.Identifier $ scmPrefixed "vector-length", x' ]
  --       OpIsTag qi ->
  --         S.app
  --           (S.Identifier $ S.recordTypePredicate $ flattenQualified currentModule qi)
  --           x'
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
          S.FunCall (Just $ atomLiteral "array") (atomLiteral "get") [ x', y' ]
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
        -- S.List [ opFixNum o', x', y' ]
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
          S.FunCall (Just $ atomLiteral "unicode") (atomLiteral "characters_to_binary")
            [ S.List [ x', y' ]
            , S.atomLiteral "utf8"
            ]
        OpStringOrd o' -> opOrd o' x' y'

-- codegenPrimEffect :: CodegenEnv -> BackendEffect NeutralExpr -> ErlExpr
-- codegenPrimEffect codegenEnv = case _ of
--   EffectRefNew v ->
--     S.app (S.Identifier $ scmPrefixed "box") (codegenExpr codegenEnv v)
--   EffectRefRead r ->
--     S.app (S.Identifier $ scmPrefixed "unbox") (codegenExpr codegenEnv r)
--   EffectRefWrite r v ->
--     S.List
--       [ S.Identifier $ scmPrefixed "set-box!", codegenExpr codegenEnv r, codegenExpr codegenEnv v ]

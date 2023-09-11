module PureScript.Backend.Erl.Convert where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.CodePoint.Unicode as StringCP
import Data.Foldable (foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.Tuple (Tuple(..), snd, uncurry)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Erl.Constants as C
import PureScript.Backend.Erl.Parser (ForeignDecls)
import PureScript.Backend.Erl.Syntax (ErlDefinition(..), ErlExport(..), ErlExpr, ErlModule, atomLiteral)
import PureScript.Backend.Erl.Syntax as S
import PureScript.Backend.Optimizer.Convert (BackendModule, BackendBindingGroup)
import PureScript.Backend.Optimizer.CoreFn (Ident(..), Literal(..), ModuleName, Prop(..), Qualified(..))
import PureScript.Backend.Optimizer.Semantics (NeutralExpr)
import PureScript.Backend.Optimizer.Syntax (BackendAccessor(..), BackendOperator(..), BackendOperator1(..), BackendOperator2(..), BackendOperatorNum(..), BackendOperatorOrd(..), BackendSyntax(..), Level(..), Pair(..))

type CodegenEnv =
  { currentModule :: ModuleName
  }

codegenModule :: BackendModule -> ForeignDecls -> ErlModule
codegenModule { name, bindings, imports, foreign: foreign_ } foreigns =
  let
    codegenEnv :: CodegenEnv
    codegenEnv = { currentModule: name }

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

-- [ Define i $ codegenExpr codegenEnv n ]

codegenExpr :: CodegenEnv -> NeutralExpr -> ErlExpr
codegenExpr codegenEnv@{ currentModule } s = case unwrap s of
  Var (Qualified (Just mn) (Ident i)) ->
    S.FunCall (Just (S.atomLiteral $ erlModuleNamePs mn)) (S.atomLiteral i) []

  Var (Qualified (Nothing) (Ident i)) ->
    S.Var i

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
  LetRec lvl bindings expr | [ binding ] <- NEA.toArray bindings ->
    S.Unimplemented "letrec1"
  LetRec lvl bindings expr ->
    S.Unimplemented "letrec"
  --     S.Let true (map (bimap (flip toErlIdent lvl <<< Just) (codegenExpr codegenEnv)) bindings) $
  --       codegenExpr codegenEnv expr
  Let i lvl e e' ->
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
  PrimEffect _ -> S.Fun Nothing [ Tuple (S.FunHead [] Nothing) (S.Tupled []) ] -- S.Unimplemented "prim_effect"
  --     codegenEffectChain codegenEnv s

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
codegenChain chainMode codegenEnv = collect []
  where
  -- `expression` has type `Effect ..`, so we can confidently unthunk here
  codegenEffectBind :: NeutralExpr -> ErlExpr
  codegenEffectBind expression = case unwrap expression of
    -- PrimEffect e' ->
    --   codegenPrimEffect codegenEnv e'
    UncurriedEffectApp f p ->
      S.FunCall Nothing (codegenExpr codegenEnv f) (codegenExpr codegenEnv <$> p)
    _ ->
      S.unthunk $ codegenExpr codegenEnv expression

  finish :: Boolean -> Array _ -> NeutralExpr -> ErlExpr
  finish shouldUnthunk bindings expression = do
    let
      maybeUnthunk :: ErlExpr -> ErlExpr
      maybeUnthunk = if shouldUnthunk then S.unthunk else identity
      res = maybeUnthunk $ codegenExpr codegenEnv expression
    if Array.null bindings then
      res
    else
      S.Block $ (uncurry S.Match <<< lmap S.Var <$> bindings) `Array.snoc` res

  collect :: Array _ -> NeutralExpr -> ErlExpr
  collect bindings expression = case unwrap expression of
    Let i l v e' ->
      collect (Array.snoc bindings $ Tuple (toErlVar i l) (codegenExpr codegenEnv v)) e'
    EffectPure e' | chainMode.effect ->
      finish false bindings e'
    -- PrimEffect e' | chainMode.effect ->
    --   codegenPrimEffect codegenEnv e'
    EffectBind i l v e' | chainMode.effect ->
      collect
        (Array.snoc bindings $ Tuple (toErlVar i l) (codegenEffectBind v))
        e'
    EffectDefer e' | chainMode.effect ->
      collect bindings e'
    _ ->
      finish chainMode.effect bindings expression

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
        S.FunCall (Just $ atomLiteral C.array) (atomLiteral C.length) [ x' ]
      OpIsTag _ ->
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

-- codegenPrimEffect :: CodegenEnv -> BackendEffect NeutralExpr -> ErlExpr
-- codegenPrimEffect codegenEnv = case _ of
--   EffectRefNew v ->
--     S.app (S.Identifier $ scmPrefixed "box") (codegenExpr codegenEnv v)
--   EffectRefRead r ->
--     S.app (S.Identifier $ scmPrefixed "unbox") (codegenExpr codegenEnv r)
--   EffectRefWrite r v ->
--     S.List
--       [ S.Identifier $ scmPrefixed "set-box!", codegenExpr codegenEnv r, codegenExpr codegenEnv v ]

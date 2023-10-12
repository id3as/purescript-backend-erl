module PureScript.Backend.Erl.Printer where

import Prelude

import Data.Array (all, foldr, intercalate)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.CodePoint.Unicode as CodePointU
import Data.CodePoint.Unicode as U
import Data.Enum (fromEnum)
import Data.Foldable (class Foldable, foldMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Monoid as M
import Data.Set as Set
import Data.String (CodePoint, toCodePointArray)
import Data.String as CodePoints
import Data.String as String
import Data.String.CodeUnits as StringCU
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.String.Regex.Unsafe as Regex.Unsafe
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Dodo (Doc, flexAlt)
import Dodo as D
import Dodo.Common (leadingComma, trailingComma)
import Partial.Unsafe (unsafePartial)
import PureScript.Backend.Erl.Syntax (BinaryOperator, CaseClause(..), ErlDefinition, ErlExport(..), ErlExpr, ErlModule, FunHead(..), Guard(..), IfClause(..), UnaryOperator)
import PureScript.Backend.Erl.Syntax as S
import Safe.Coerce (coerce)

printWrap :: Doc Void -> Doc Void -> Doc Void -> Doc Void
printWrap l r x = l <> x <> r

printParens :: Doc Void -> Doc Void
printParens = printWrap (D.text "(") (D.text ")")

printBrackets :: Doc Void -> Doc Void
printBrackets = printWrap (D.text "[") (D.text "]")

printBraces :: Doc Void -> Doc Void
printBraces = printWrap (D.text "{") (D.text "}")

printWrap' :: Doc Void -> Doc Void -> Array (Doc Void) -> Doc Void
printWrap' l r [] = l <> r
printWrap' l r [x] = l <> x <> r
printWrap' l r xs = D.flexGroup $
  D.alignCurrentColumn (l <> D.flexAlt mempty D.space <> commaSepLines xs <> D.softBreak <> r)

printWrap_ :: Doc Void -> Doc Void -> Array (Doc Void) -> Doc Void
printWrap_ l r [] = l <> r
printWrap_ l r xs = D.flexGroup $
  D.alignCurrentColumn (l <> D.space <> commaSepLines xs <> D.spaceBreak <> r)

printParens' :: Array (Doc Void) -> Doc Void
printParens' = printWrap' (D.text "(") (D.text ")")

printOptParens' :: Array (Doc Void) -> Doc Void
printOptParens' [x] = x
printOptParens' xs = printWrap' (D.text "(") (D.text ")") xs

printParens_ :: Array (Doc Void) -> Doc Void
printParens_ = printWrap_ (D.text "(") (D.text ")")

printBrackets' :: Array (Doc Void) -> Doc Void
printBrackets' = printWrap' (D.text "[") (D.text "]")

printBrackets_ :: Array (Doc Void) -> Doc Void
printBrackets_ = printWrap_ (D.text "[") (D.text "]")

printBraces' :: Array (Doc Void) -> Doc Void
printBraces' = printWrap' (D.text "{") (D.text "}")

printBraces_ :: Array (Doc Void) -> Doc Void
printBraces_ = printWrap_ (D.text "{") (D.text "}")

commaSep :: forall f. Foldable f => f (Doc Void) -> Doc Void
commaSep = D.foldWithSeparator (D.text ", ")

commaSepLines :: forall f. Foldable f => f (Doc Void) -> Doc Void
commaSepLines = D.foldWithSeparator leadingComma

commaSepMap :: forall f a. Traversable f => (a -> Doc Void) -> f a -> Doc Void
commaSepMap f = commaSep <<< map f

maybeSep :: forall a. (a -> Doc Void) -> Maybe a -> Doc Void -> Doc Void
maybeSep _ Nothing _ = mempty
maybeSep f (Just a) sep = f a <> sep

sepMaybe :: forall a. Doc Void -> (a -> Doc Void) -> Maybe a -> Doc Void
sepMaybe _ _ Nothing = mempty
sepMaybe sep f (Just a) = sep <> f a

linesSeparated :: Prim.Array (Doc Void) -> Doc Void
linesSeparated = Array.intercalate twoLineBreaks
  where
  twoLineBreaks = D.break <> D.break


printModule :: ErlModule -> Doc Void
printModule lib =
  flip append D.break
    $ D.lines $
      [ printAttribute "module" [ D.text (escapeAtom lib.moduleName) ]
      , printAttribute "export" $ pure $ printBrackets' $
          lib.exports <#> \(Export name arity) ->
            D.text (escapeAtom name) <> D.text "/" <> D.text (show arity)
      , printAttribute "compile" [ D.text "no_auto_import" ]
      , printMacrosFor lib
      ]
      <>
      map printDefinition lib.definitions

printMacrosFor :: ErlModule -> Doc Void
printMacrosFor { definitions } =
  let used = foldMap (\(S.FunctionDefinition _ _ e) -> S.macros e) definitions in
  S.predefMacros # foldMap \(Tuple (Tuple name args) def) ->
    if not Set.member name used then mempty else
    printAttribute "define"
      [ D.text name <> foldMap (printParens' <<< NEA.toArray <<< map D.text) args
      , printExpr def
      ]

printAttribute :: String -> Array (Doc Void) -> Doc Void
printAttribute name a = D.text "-" <> D.text name <> printParens' a <> D.text "."

printDefinition :: ErlDefinition -> Doc Void
printDefinition = case _ of
  S.FunctionDefinition name args e ->
    D.text (escapeAtom name) <> printParens (commaSep $ D.text <$> args) <> D.text " ->"
      <> D.break <> D.indent (printAtomic e)
      <> D.text "." <> D.break

type Precedence = Boolean
atomic = true :: Precedence

parenPrec :: Precedence -> Doc Void -> Doc Void
parenPrec p d | p == atomic = d
parenPrec _ d = D.flexGroup $ printParens d

printExpr :: ErlExpr -> Doc Void
printExpr e = printExpr' false e

printAtomic :: ErlExpr -> Doc Void
printAtomic e = D.flexGroup $ printExpr' atomic e

printExpr' :: Precedence -> ErlExpr -> Doc Void
printExpr' prec = case _ of
  S.Literal (S.Integer n) -> D.text $ show n
  S.Literal (S.Float f) -> D.text $
    -- Erlang does not like scientific notation without a decimal point
    show f # Regex.replace' (Regex.Unsafe.unsafeRegex "^(\\d+)(e[+-]?\\d+)$" mempty) \original ->
      case _ of
        [Just integer, Just exponent] -> integer <> ".0" <> exponent
        _ -> original
  S.Literal (S.Char c) -> D.text $ "$" <> escapeNonprinting (escapeErlString (StringCU.singleton c))
  S.Literal (S.Atom a) -> D.text $ escapeAtom a

  s@(S.BinaryAppend _ _) -> printBinaryLiteral s
  s@(S.Literal (S.String _)) -> printBinaryLiteral s

  S.Var v -> D.text v

  S.List a -> printBrackets' $ D.indent <<< printAtomic <$> a
  S.ListCons [] rest -> printExpr' prec rest
  S.ListCons a rest -> printBrackets_ $ a # mapWithIndex \i item ->
    D.indent (printAtomic item)
      <> if i /= Array.length a - 1 then mempty else
        D.flexAlt (D.space <> D.text "|") (D.break <> D.text "|") <> D.space <> D.indent (printAtomic rest)
  S.Tupled a -> printBraces' $ D.indent <<< printAtomic <$> a
  S.Map fields -> D.text "#" <> printBraces_ (D.indent <<< printField <$> fields)
  S.MapPattern fields -> D.text "#" <> printBraces_ (D.indent <<< printFieldPattern <$> fields)
  S.MapUpdate e fields | tiny e -> printAtomic e <> D.text "#" <> printBraces_ (printField <$> fields)
  S.MapUpdate e fields -> D.text "(" <> printAtomic e <> D.text ")#" <> printBraces_ (printField <$> fields)

  S.Match e1 e2 -> printExpr e1 <> D.text " = " <> printExpr e2

  S.Block exprs ->
    D.text "begin" <> D.break <>
      D.indent (D.foldWithSeparator trailingComma (printAtomic <<< skipUnusedMatch <$> exprs)) <> D.break <>
    D.text "end"

  S.Fun name heads -> parenPrec prec $ do
    let
      printFunHead :: Tuple FunHead ErlExpr -> Doc Void
      printFunHead (Tuple (FunHead exprs g) e) =
        maybeSep D.text name D.space <>
          printParens' (printExpr <$> exprs) <>
          sepMaybe (D.text " when ") (coerce printAtomic) g <>
          D.text " ->" <> D.break <>
          D.indent (printAtomic e) <> D.break

    D.text "fun" <> D.break
      <> D.indent (
        D.lines $ printFunHead <$> heads
      )
      <> D.text "end"

  S.FunCall qualifier function [] ->
    parenPrec prec $
      maybeSep printExpr qualifier (D.text ":")
      <> printExpr function
      <> D.text "()"
  S.FunCall qualifier function [arg] ->
    parenPrec prec $ D.alignCurrentColumn $
      maybeSep printExpr qualifier (D.text ":")
      <> printExpr function
      <> M.guard (not tiny function) D.softBreak
      <> printParens (printAtomic arg)
  S.FunCall qualifier function args ->
    parenPrec prec $ D.alignCurrentColumn $
      maybeSep printExpr qualifier (D.text ":")
      <> printExpr function
      <> M.guard (not tiny function) D.softBreak
      <> printParens (D.softBreak <> intercalate trailingComma (D.indent <<< printAtomic <$> args) <> D.softBreak)

  S.If clauses ->
    D.text "if" <> D.break <>
      D.indent (D.foldWithSeparator trailingSemi (printIfClause <$> clauses))
      <> D.break <>
    D.text "end"

  S.Case expr clauses ->
    -- extra indent to clear the indenting of each case clause
    D.text "case " <> D.indent (printAtomic expr) <> D.text " of" <> D.break <>
      D.indent (D.foldWithSeparator trailingSemi (printCaseClause <$> clauses))
      <> D.break <>
    D.text "end"

  S.BinOp op e1 e2 ->
    parenPrec prec $
      printExpr e1 <> D.spaceBreak <> D.indent (printBinOp op <> D.space <> printExpr e2)

  S.UnaryOp op e1 ->
    parenPrec prec $
      printUnaryOp op <> D.space <> printExpr e1

  S.Macro name args ->
    D.text ("?" <> name) <> foldMap (printParens <<< commaSepMap printAtomic) args

tiny :: ErlExpr -> Boolean
tiny (S.Var _) = true
tiny (S.Literal (S.Integer _)) = true
tiny (S.Literal (S.Atom _)) = true
tiny _ = false

printIfClause :: IfClause -> Doc Void
printIfClause (IfClause guard e) =
  printAtomic guard <> D.text " ->"  <> D.break <> D.indent (printAtomic e)

printCaseClause :: CaseClause -> Doc Void
printCaseClause (CaseClause test guard e) =
  printAtomic test <> sepMaybe (D.text " when ") (coerce printAtomic) guard <> D.text " ->" <> D.break <> D.indent (printAtomic e)

trailingSemi :: forall a. Doc a
trailingSemi = flexAlt (D.text "; ") (D.text ";" <> D.break)

escapeAtom :: String -> String
escapeAtom a =
  if isValidAtom
    then a
    else "'" <> (foldMap replaceChar $ String.toCodePointArray a) <> "'"
  where
  replaceChar c | c == CodePoints.codePointFromChar '\'' = "\\'"
  -- TODO: hex format
  -- replaceChar c | not (CodePointU.isLatin1 c) = "@x" <> hex 4 c
  replaceChar c = String.singleton c

  atomCP c | c == String.codePointFromChar '_' = true
  atomCP c | c == String.codePointFromChar '@' = true
  atomCP c = CodePointU.isDecDigit c || (CodePointU.isLatin1 c && CodePointU.isAlpha c)

  isValidAtom = case String.uncons a of
    Nothing -> false
    Just { head } ->
      CodePointU.isLower head
        && Array.all atomCP (String.toCodePointArray a)
        && not (nameIsErlReserved a)

skipUnusedMatch :: ErlExpr -> ErlExpr
skipUnusedMatch (S.Match (S.Var "_") e) = e
skipUnusedMatch e = e

nameIsErlReserved :: String -> Boolean
nameIsErlReserved name =
  name `Array.elem` erlAnyReserved

erlAnyReserved :: Array String
erlAnyReserved = [
  "after",
  "and",
  "andalso",
  "band",
  "begin",
  "bnot",
  "bor",
  "bsl",
  "bsr",
  "bxor",
  "case",
  "catch",
  "cond",
  "div",
  "end",
  "fun",
  "if",
  "let",
  "not",
  "of",
  "or",
  "orelse",
  "receive",
  "rem",
  "try",
  "when",
  "xor"
  ]


printBinOp :: BinaryOperator -> Doc Void
printBinOp = D.text <<< case _ of
  -- These are arranged according to precedence table
  S.FDivide              -> "/"
  S.IDivide              -> "div"
  S.Multiply             -> "*"
  S.Remainder            -> "rem"
  S.BitwiseAnd           -> "band"

  S.Add                  -> "+"
  S.Subtract             -> "-"
  S.BitwiseOr            -> "bor"
  S.BitwiseXor           -> "bxor"
  S.ShiftLeft            -> "bsl"
  S.ShiftRight           -> "bsr"
  S.XOr                  -> "xor"

  S.ListConcat           -> "++"
  S.ListSubtract         -> "--"

  S.EqualTo              -> "=="
  S.NotEqualTo           -> "/="
  S.IdenticalTo          -> "=:="
  S.NotIdenticalTo       -> "=/="
  S.LessThan             -> "<"
  S.LessThanOrEqualTo    -> "=<"
  S.GreaterThan          -> ">"
  S.GreaterThanOrEqualTo -> ">="

  S.AndAlso              -> "andalso"

  S.OrElse               -> "orelse"

printUnaryOp :: UnaryOperator -> Doc Void
printUnaryOp = D.text <<< case _ of
  S.Not ->                  "not"
  S.BitwiseNot ->           "bnot"
  S.Positive ->             "+"
  S.Negate ->               "-"

printBinaryLiteral :: ErlExpr -> Doc Void
printBinaryLiteral = finish <<< go
  where
  finish items =
    D.text "<<" <> Array.intercalate (D.text ", ") items <> D.text ">>"
  go (S.BinaryAppend e1 e2) = go e1 <> go e2
  go (S.Literal (S.String s))
    | isAscii s = [ D.text $ "\"" <> escapeErlString s <> "\"" ]
    | otherwise = [ D.text $ "\"" <> escapeErlString s <> "\"/utf8" ]
  go s = [ printExpr s <> D.text "/binary" ]

printField :: Tuple String ErlExpr -> Doc Void
printField (Tuple f e) =
  D.text (escapeAtom f) <> D.text " =>" <> D.flexGroup (D.spaceBreak <> printAtomic e)

printFieldPattern :: Tuple String ErlExpr -> Doc Void
printFieldPattern (Tuple f e) =
  D.text (escapeAtom f) <> D.text " :=" <> D.flexGroup (D.spaceBreak <> printAtomic e)

isAscii :: String -> Boolean
isAscii = toCodePointArray >>> all U.isAscii

erlEscapes :: Array (Tuple CodePoint String)
erlEscapes = map (\(s /\ r) -> unsafePartial (let [ c ] = toCodePointArray s in c) /\ ("\\" <> r))
  -- https://www.erlang.org/doc/reference_manual/data_types#escape-sequences
  [ "\x08" /\ "b"
  , "\x7F" /\ "d"
  , "\x1B" /\ "e"
  , "\x0C" /\ "f"
  , "\n" /\ "n"
  , "\r" /\ "r"
  -- Can we not
  -- , " " /\ "s"
  , "\t" /\ "t"
  , "\x0B" /\ "v"
  , "\x0" /\ "x{0}" -- or "^@"
  , "'" /\ "'"
  , "\"" /\ "\""
  , "\\" /\ "\\"
  ]

escapeErlString :: String -> String
escapeErlString = foldr (\(c /\ r) -> String.replaceAll (String.Pattern (String.singleton c)) (String.Replacement r)) <@> erlEscapes

escapeNonprinting :: String -> String
escapeNonprinting = Regex.replace'
  (Regex.Unsafe.unsafeRegex "[^ -~]" (Regex.Flags.unicode <> Regex.Flags.multiline <> Regex.Flags.global))
  \s _ -> toHexEscape s

toHexEscape :: String -> String
toHexEscape s =
  case toCodePointArray s of
    [ c ] -> "\\x{" <> show (fromEnum c) <> "}"
    _ -> s

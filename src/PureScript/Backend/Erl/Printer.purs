module PureScript.Backend.Erl.Printer where

import Prelude

import Data.Array (all, foldr)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.CodePoint.Unicode as CodePointU
import Data.CodePoint.Unicode as U
import Data.Enum (fromEnum)
import Data.Foldable (class Foldable, fold, foldMap)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
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
import Dodo.Common (trailingComma)
import Partial.Unsafe (unsafePartial)
import PureScript.Backend.Erl.Constants as C
import PureScript.Backend.Erl.Syntax (BinaryOperator, CaseClause(..), ErlDefinition, ErlExport(..), ErlExpr, ErlModule, FunHead(..), IfClause(..), UnaryOperator, atomLiteral)
import PureScript.Backend.Erl.Syntax as S

printWrap :: Doc Void -> Doc Void -> Doc Void -> Doc Void
printWrap l r x = l <> x <> r

printParens :: Doc Void -> Doc Void
printParens = printWrap (D.text "(") (D.text ")")

printBrackets :: Doc Void -> Doc Void
printBrackets = printWrap (D.text "[") (D.text "]")

printBraces :: Doc Void -> Doc Void
printBraces = printWrap (D.text "{") (D.text "}")

commaSep :: forall f. Foldable f => f (Doc Void) -> Doc Void
commaSep = D.foldWithSeparator (D.text ", ")

commaSepMap :: forall f a. Traversable f => (a -> Doc Void) -> f a -> Doc Void
commaSepMap f = commaSep <<< map f

linesSeparated :: Prim.Array (Doc Void) -> Doc Void
linesSeparated = Array.intercalate twoLineBreaks
  where
  twoLineBreaks = D.break <> D.break


printModule :: ErlModule -> Doc Void
printModule lib =
  flip append D.break
    $ D.lines $
      [ printAttribute "module" (D.text (escapeAtom lib.moduleName))
      , printAttribute "export" $ printBrackets $
          commaSep $ (\(Export name arity) -> D.text (escapeAtom name) <> D.text "/" <> D.text (show arity)) <$> lib.exports
      , printAttribute "compile" (D.text "no_auto_import")
      , printMacrosFor lib
      ]
      <>
      map printDefinition lib.definitions

printMacrosFor :: ErlModule -> Doc Void
printMacrosFor { definitions } =
  let used = foldMap (\(S.FunctionDefinition _ _ e) -> S.macros e) definitions in
  S.predefMacros # foldMap \(Tuple (Tuple name args) def) ->
    if not Set.member name used then mempty else
    printAttribute "define" $ commaSep
      [ D.text name <> foldMap (printParens <<< commaSepMap D.text) args
      , printExpr def
      ]

printAttribute :: String -> Doc Void -> Doc Void
printAttribute name a = D.text "-" <> D.text name <> printParens a <> D.text "."

printDefinition :: ErlDefinition -> Doc Void
printDefinition = case _ of
  S.FunctionDefinition name args e ->
    D.text (escapeAtom name) <> printParens (commaSep $ D.text <$> args) <> D.text " ->"
      <> D.break <> D.indent (printExpr e)
      <> D.text "."


printExpr :: ErlExpr -> Doc Void
printExpr = case _ of
  S.Literal (S.Integer n) -> D.text $ show n
  S.Literal (S.Float f) -> D.text $
    -- Erlang does not like scientific notation without a decimal point
    show f # Regex.replace' (Regex.Unsafe.unsafeRegex "^(\\d+)(e[+-]?\\d+)$" mempty) \original ->
      case _ of
        [Just integer, Just exponent] -> integer <> ".0" <> exponent
        _ -> original
  S.Literal (S.String s)
    | isAscii s -> D.text $ "<<\"" <> escapeErlString s <> "\">>"
    | otherwise -> D.text $ "<<\"" <> escapeErlString s <> "\"/utf8>>"
  S.Literal (S.Char c) -> D.text $ "$" <> escapeNonprinting (escapeErlString (StringCU.singleton c))
  S.Literal (S.Atom a) -> D.text $ escapeAtom a

  s@(S.BinaryAppend _ _) -> printBinaryAppend s

  S.Var v -> D.text v

  S.List a -> printBrackets $ commaSep $ printExpr <$> a
  S.ListCons a rest -> printBrackets $ fold
    [ commaSep $ printExpr <$> a
    , D.text "|"
    , printExpr rest
    ]
  S.Tupled a -> printBraces $ commaSep $ printExpr <$> a
  S.Map fields -> D.text "#{" <> D.foldWithSeparator (D.text "," <> D.spaceBreak) (printField <$> fields) <> D.text "}"
  S.MapUpdate e fields -> D.text "(" <> printExpr e <> D.text ")#{" <> D.foldWithSeparator (D.text "," <> D.spaceBreak) (printField <$> fields) <> D.text "}"

  S.Match e1 e2 -> printExpr e1 <> D.text " = " <> printExpr e2

  S.Block exprs ->
    D.text "begin" <> D.break <>
      D.indent (D.foldWithSeparator trailingComma (printExpr <$> exprs)) <> D.break <>
    D.text "end"

  S.Fun name heads -> printParens $ do
    let
      printFunHead :: Tuple FunHead ErlExpr -> Doc Void
      printFunHead (Tuple (FunHead exprs g) e) =
        maybe mempty (\n -> D.text n <> D.space) name <>
          printParens (commaSep $ printExpr <$> exprs) <>
          D.text " ->" <> D.break <>
          D.indent (printExpr e) <> D.break

    D.text "fun" <> D.break
      <> D.indent (
        D.lines $ printFunHead <$> heads
      )
      <> D.text "end"

  S.FunCall qualifier function args ->
    printParens $
      maybe mempty (\q -> printExpr q <> D.text ":")  qualifier
      <> printExpr function
      <> printParens (commaSep $ printExpr <$> args)

  S.If clauses ->
    D.text "if" <> D.break <>
      D.indent (D.foldWithSeparator trailingSemi (printIfClause <$> clauses))
      <> D.break <>
    D.text "end"

  S.Case expr clauses ->
    D.text "case " <> printExpr expr <> D.text " of" <> D.break <>
      D.indent (D.foldWithSeparator trailingSemi (printCaseClause <$> clauses))
      <> D.break <>
    D.text "end"

  S.BinOp op e1 e2 ->
    printParens $
      printExpr e1 <> D.space <> printBinOp op <> D.space <> printExpr e2

  S.UnaryOp op e1 ->
    printParens $
      printUnaryOp op <> D.space <> printExpr e1

  S.Macro name args ->
    D.text ("?" <> name) <> foldMap (printParens <<< commaSepMap printExpr) args

printIfClause :: IfClause -> Doc Void
printIfClause (IfClause guard e) =
  printExpr guard <> D.text " ->"  <> D.break <> D.indent (printExpr e)

printCaseClause :: CaseClause -> Doc Void
printCaseClause (CaseClause test guard e) =
  printExpr test  <> maybe mempty (\g -> D.text " when " <> printExpr g) guard<> D.text " ->" <> D.break <> D.indent (printExpr e)

trailingSemi :: forall a. Doc a
trailingSemi = flexAlt (D.text "; ") (D.text ";" <> D.break)

escapeAtom :: String -> String
escapeAtom a =
  if isValidAtom a
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

  isValidAtom a = case String.uncons a of
    Nothing -> false
    Just { head, tail }-> CodePointU.isLower head && Array.all atomCP (String.toCodePointArray a) && not (nameIsErlReserved a)

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
  S.And                  -> "and"

  S.Add                  -> "+"
  S.Subtract             -> "-"
  S.BitwiseOr            -> "bor"
  S.BitwiseXor           -> "bxor"
  S.ShiftLeft            -> "bsl"
  S.ShiftRight           -> "bsr"
  S.Or                   -> "or"
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

printBinaryAppend :: ErlExpr -> Doc Void
printBinaryAppend = finish <<< go
  where
  finish items =
    D.text "<<" <> Array.intercalate (D.text ", ") (items <#> \e -> printExpr e <> D.text "/binary") <> D.text ">>"
  go (S.BinaryAppend e1 e2) = go e1 <> go e2
  go s = [ s ]

printField :: Tuple String ErlExpr -> Doc Void
printField (Tuple f e) =
  D.text (escapeAtom f) <> D.text " => " <> printExpr e

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

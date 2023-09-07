module PureScript.Backend.Erl.Printer where

import Prelude

import Data.Array as Array
import Data.CodePoint.Unicode as CodePointU
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), maybe)
import Data.String (CodePoint)
import Data.String as CodePoints
import Data.String as String
import Data.String.CodeUnits as StringCU
import Data.Tuple (Tuple(..))
import Dodo (Doc, flexAlt)
import Dodo as D
import Dodo.Common (trailingComma)
import PureScript.Backend.Erl.Syntax (BinaryOperator, CaseClause(..), ErlDefinition, ErlExpr, ErlModule, FunHead(..), IfClause(..), UnaryOperator)
import PureScript.Backend.Erl.Syntax as S

printWrap :: Doc Void -> Doc Void -> Doc Void -> Doc Void
printWrap l r x = l <> x <> r

printParens :: Doc Void -> Doc Void
printParens = printWrap (D.text "(") (D.text ")")

printBrackets :: Doc Void -> Doc Void
printBrackets = printWrap (D.text "[") (D.text "]")

printBraces :: Doc Void -> Doc Void
printBraces = printWrap (D.text "{") (D.text "}")

linesSeparated :: Prim.Array (Doc Void) -> Doc Void
linesSeparated = Array.intercalate twoLineBreaks
  where
  twoLineBreaks = D.break <> D.break


printModule :: ErlModule -> Doc Void
printModule lib =
  flip append D.break
    $ D.lines $
      [ printAttribute "module" (D.text lib.moduleName)
      ]
      <>
      map printDefinition lib.definitions


printAttribute :: String -> Doc Void -> Doc Void
printAttribute name a = D.text "-" <> D.text name <> printParens a <> D.text "."

printDefinition :: ErlDefinition -> Doc Void
printDefinition = case _ of
  S.UnimplementedDefinition -> D.text "?"
  S.FunctionDefinition name args e ->
    D.text (escapeAtom name) <> printParens (D.foldWithSeparator (D.text ", ") $ D.text <$> args) <> D.text " -> "
      <> D.break <> D.indent (printExpr e)
      <> D.text "."


printExpr :: ErlExpr -> Doc Void
printExpr = case _ of
  S.Literal (S.Integer n) -> D.text $ show n
  S.Literal (S.Float f) -> D.text $ show f
  S.Literal (S.String s) -> D.text $ "<<\"" <> s <> "\">>"
  S.Literal (S.Char c) -> D.text $ "$" <> StringCU.singleton c
  S.Literal (S.Atom a) -> D.text $ escapeAtom a

  S.Var v -> D.text v

  S.List a -> printBrackets $ D.foldWithSeparator (D.text ",") $ printExpr <$> a
  S.Tupled a -> printBraces $ D.foldWithSeparator (D.text ",") $ printExpr <$> a
  S.Map fields -> D.text "#{" <> D.foldWithSeparator (D.text ",") (printField <$> fields) <> D.text "}"
  S.MapUpdate e fields -> D.text "(" <> printExpr e <> D.text ")#{" <> D.foldWithSeparator (D.text ",") (printField <$> fields) <> D.text "}"

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
          printParens (D.foldWithSeparator (D.text ", ") $ printExpr <$> exprs) <>
          D.text " -> " <> D.break <>
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
      <> printParens (D.foldWithSeparator (D.text ", ") $ printExpr <$> args)

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

  S.Unimplemented s -> D.text ("?"<>s)

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

printField :: Tuple String ErlExpr -> Doc Void
printField (Tuple f e) =
  D.text f <> D.text " => " <> printExpr e

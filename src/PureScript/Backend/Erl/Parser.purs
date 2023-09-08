module PureScript.Backend.Erl.Parser (ForeignDecls, parseFile) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (notElem)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (fold)
import Data.Int as Int
import Data.List ((:))
import Data.Maybe (fromJust)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Parsing (ParseError, Parser, runParser) as P
import Parsing.Combinators (between, many, many1, sepBy, skipMany, try) as P
import Parsing.String as PC
import Parsing.String.Basic as PB
import Partial.Unsafe (unsafePartial)

type ForeignDecls =
  { exported :: Array (Tuple String Int)
  , ignored :: Array (Tuple String Int)
  }

parseFile :: String -> Either P.ParseError ForeignDecls
parseFile = P.runParser <@> parseLines

parseLines :: P.Parser String ForeignDecls
parseLines = do
  l <- parseLine
  lns <- P.many $ do
    _ <- PC.char '\n' <|> (PC.char '\r' *> PC.char '\n')
    parseLine
  PC.eof
  pure $ fold $ [ l ] <> Array.fromFoldable lns

parseLine :: P.Parser String ForeignDecls
parseLine = ({ exported: _, ignored: [] } <$> P.try (parseAttribute "export")) <|>
            ({ exported: [], ignored: _ } <$> P.try (parseAttribute "purs_ignore_exports")) <|>
  do
    P.skipMany $ PC.satisfy $ flip notElem ['\n', '\r']
    pure mempty

parseAttribute :: String -> P.Parser String (Array (Tuple String Int))
parseAttribute text = Array.fromFoldable <$> attributeParser text
  (P.between (PC.char '[') (PC.char ']') (atomArityParser `P.sepBy` PC.char ','))

-- P.Parsec String u Token
--
attributeParser :: forall a. String -> P.Parser String a -> P.Parser String a
attributeParser name valueParser =
  -- PC.char '-' *> PC.string name *> P.between (PC.char '(') (PC.char ')') valueParser
  do
    _ <- PB.whiteSpace
    _ <- PC.char '-'
    _ <- PB.whiteSpace
    _ <- PC.string name
    _ <- PB.whiteSpace
    res <- P.between (PC.char '(' *> PB.whiteSpace) (PB.whiteSpace *> PC.char ')') valueParser
    _ <- PB.whiteSpace
    _ <- PC.char '.'
    P.skipMany $ PC.satisfy $ flip notElem ['\n', '\r']
    pure res

atomArityParser :: P.Parser String (Tuple String Int)
atomArityParser = do
  _ <- PB.whiteSpace
  a <- atomParser
  _ <- PB.whiteSpace
  _ <- PC.char '/'
  _ <- PB.whiteSpace
  n <- (\digits -> unsafePartial (fromJust (Int.fromString digits))) <<< fromCharArray <<< Array.fromFoldable <$> P.many1 PB.digit
  _ <- PB.whiteSpace
  pure (Tuple a n)

atomParser :: P.Parser String String
atomParser = quotedAtomParser <|> identifierParser

identifierParser :: P.Parser String String
identifierParser = do
  h <- PB.lower
  t <- P.many (PB.alphaNum <|> PC.char '_' <|> PC.char '@')
  pure $ fromCharArray $ Array.fromFoldable $ h : t

quotedAtomParser :: P.Parser String String
quotedAtomParser = P.between (PC.char '\'') (PC.char '\'')
  (fromCharArray <<< Array.fromFoldable <$> P.many1 (PC.satisfy (flip notElem ['\'', '\\']) <|> atomEscapedCharParser))

atomEscapedCharParser :: P.Parser String Char
atomEscapedCharParser = do
  _ <- PC.char '\\'
  PC.char '\'' <|> PC.char '\\'

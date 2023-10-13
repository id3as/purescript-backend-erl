module PureScript.Backend.Erl.Convert.Common where

import Prelude

import Data.CodePoint.Unicode as StringCP
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.Tuple (Tuple, uncurry)
import PureScript.Backend.Erl.Syntax (ErlExpr, ErlPattern, self)
import PureScript.Backend.Erl.Syntax as S
import PureScript.Backend.Optimizer.CoreFn (Ident, ModuleName)
import PureScript.Backend.Optimizer.Syntax (Level(..))

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
toErlVar text lvl =
  maybe "V" (toErlVarName <<< unwrap) text # addLevel lvl

addLevel :: Level -> String -> String
addLevel (Level 0) = identity
addLevel (Level lvl) | lvl < 0 = const "_"
addLevel (Level lvl) = (_ <> ("@" <> show lvl))

toErlVarWith :: String -> Maybe Ident -> Level -> String
toErlVarWith suffix text lvl =
  maybe "V" (toErlVarName <<< unwrap) text <> "@" <> suffix # addLevel lvl

-- String.replace (String.Pattern ".") (String.Replacement "_") (unwrap name)
--   T.intercalate "_" (toAtomName <$> T.splitOn "." name)

toErlVarName :: String -> String
toErlVarName text = case String.uncons text of
  Just { head, tail } ->
    -- Lazy to include start
    String.replaceAll (String.Pattern "'") (String.Replacement "_@prime") $
    String.replaceAll (String.Pattern "$") (String.Replacement "_@dollar") $
     String.fromCodePointArray (StringCP.toUpper head) <> tail
  Nothing -> "V"

toErlVarExpr :: Tuple (Maybe Ident) Level -> ErlExpr
toErlVarExpr = (S.Var <@> self) <<< uncurry toErlVar

toErlVarPat :: Tuple (Maybe Ident) Level -> ErlPattern
toErlVarPat = uncurry toErlVar >>> case _ of
  "_" -> S.Discard
  v -> S.BindVar v

tagAtom :: Ident -> ErlExpr
tagAtom tagName = S.Literal $ S.Atom $ toAtomName $ unwrap tagName

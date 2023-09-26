module Main where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Data.Array (notElem)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..), either)
import Data.Foldable (for_, traverse_)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (power)
import Data.Newtype (unwrap)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Dodo (plainText)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, throwError, try)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Library.Execa (execa)
import Node.Path as Path
import Node.Process as Process
import Parsing (parseErrorMessage)
import PureScript.Backend.Erl.Constants (erlExt)
import PureScript.Backend.Erl.Convert (codegenModule)
import PureScript.Backend.Erl.Convert.Common (erlModuleNamePs, erlModuleNameForeign)
import PureScript.Backend.Erl.Foreign (erlForeignSemantics)
import PureScript.Backend.Erl.Foreign.Analyze (analyzeCustom)
import PureScript.Backend.Erl.Parser (parseFile)
import PureScript.Backend.Erl.Printer as P
import PureScript.Backend.Optimizer.Builder (buildModules)
import PureScript.Backend.Optimizer.CoreFn (Ident, Module(..), ModuleName(..), Qualified(..))
import PureScript.Backend.Optimizer.Directives (parseDirectiveFile)
import PureScript.Backend.Optimizer.Directives.Defaults (defaultDirectives)
import PureScript.Backend.Optimizer.Semantics.Foreign (ForeignEval, coreForeignSemantics)
import Test.Utils (coreFnModulesFromOutput, mkdirp)

type MainArgs =
  { compile :: Boolean
  , filter :: NonEmptyArray String
  , cwd :: Maybe String
  }

argParser :: ArgParser MainArgs
argParser =
  ArgParser.fromRecord
    { compile:
        ArgParser.flag [ "--compile", "-c" ]
          "Compile generated Erlang with erlc"
          # ArgParser.boolean
          # ArgParser.default false
    , filter:
        ArgParser.argument [ "--filter", "-f" ]
          "Filter modules matching a prefix"
          # ArgParser.unfolded1
          # ArgParser.default (pure "*")
    , cwd:
        ArgParser.argument [ "--cwd" ]
          "Set cwd"
          # ArgParser.optional
    }

main :: Effect Unit
main = do
  cliArgs <- Array.drop 2 <$> Process.argv
  case ArgParser.parseArgs "test" "" argParser cliArgs of
    Left err ->
      Console.error $ ArgParser.printArgError err
    Right args ->
      launchAff_ $ runCompile args

foreignSemantics :: Map.Map (Qualified Ident) ForeignEval
foreignSemantics = Map.union erlForeignSemantics $
  coreForeignSemantics # Map.filterKeys
    \(Qualified mod _) -> mod `notElem`
      [ Just (ModuleName "Effect.Ref")
      , Just (ModuleName "Control.Monad.ST.Internal")
      ]

runCompile :: MainArgs -> Aff Unit
runCompile { compile, filter, cwd } = do
  liftEffect $ traverse_ Process.chdir cwd
  currentDir <- liftEffect Process.cwd
  let outputDir = Path.concat [ currentDir, "output-erl" ]
  let ebin = Path.concat [ outputDir, "ebin" ]
  mkdirp outputDir
  mkdirp ebin
  erls <- liftEffect $ Ref.new []
  coreFnModulesFromOutput "output" filter >>= case _ of
    Left errors -> do
      for_ errors \(Tuple filePath err) -> do
        Console.error $ filePath <> " " <> err
      liftEffect $ Process.exit 1
    Right coreFnModules -> do
      when (List.null coreFnModules) do
        Console.log "No modules; try building"
      let { directives } = parseDirectiveFile defaultDirectives
      coreFnModules # buildModules
        { directives
        , analyzeCustom
        , foreignSemantics
        , traceIdents: mempty
        , onCodegenModule: \_ (Module { name: ModuleName name, path, exports }) (backend) _ -> do
            let moduleOutputDir = Path.concat [ outputDir, name ]
            let moduleOutputPath = Path.concat [ moduleOutputDir, erlModuleNamePs (ModuleName name) <> erlExt ]
            let moduleOutputForeignPath = Path.concat [ moduleOutputDir, erlModuleNameForeign (ModuleName name) <> erlExt ]
            let
              fileForeign =
                Path.concat [ currentDir, path ]
                  # (fromMaybe <*> String.stripSuffix (String.Pattern ".purs"))
                  # (_ <> ".erl")
            foreignFile <- try $ FS.readTextFile UTF8 fileForeign
            let foreignsE = either (Right <<< mempty) parseFile foreignFile
            foreigns <- either (throwError <<< Aff.error <<< parseErrorMessage) pure foreignsE
            let
              formatted =
                Dodo.print plainText Dodo.twoSpaces
                  $ P.printModule
                  $ codegenModule backend foreigns
            mkdirp moduleOutputDir
            FS.writeTextFile UTF8 moduleOutputPath formatted
            for_ foreignFile \x -> do
              FS.writeTextFile UTF8 moduleOutputForeignPath x
              liftEffect $ Ref.modify_ (_ <> [moduleOutputForeignPath]) erls
            liftEffect $ Ref.modify_ (_ <> [moduleOutputPath]) erls
        , onPrepareModule: \build coreFnMod@(Module { name }) -> do
            let total = show build.moduleCount
            let index = show (build.moduleIndex + 1)
            let padding = power " " (SCU.length total - SCU.length index)
            Console.log $ "[" <> padding <> index <> " of " <> total <> "] Building " <> unwrap name
            pure coreFnMod
        }
      when compile do
        filesToCompile <- liftEffect $ Ref.read erls
        spawned <- execa "erlc" ([ "-W0" ] <> filesToCompile)
          _ { cwd = Just ebin }
        spawned.result >>= case _ of
          Left { message } -> do
            Console.log $ withGraphics (foreground Red) "✗ failed to compile."
            Console.log message
          Right _ -> pure unit
      pure unit

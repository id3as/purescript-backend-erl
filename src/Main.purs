module PureScript.Backend.Erl.Main where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (fold, foldMap, for_, traverse_)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (power)
import Data.Newtype (unwrap)
import Data.Ord.Max (Max(..))
import Data.Set (Set)
import Data.Set as Set
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
import Node.FS.Stats (modifiedTimeMs)
import Node.Library.Execa (execa)
import Node.Path as Path
import Node.Process as Process
import Parsing (parseErrorMessage)
import PureScript.Backend.Erl.Calling (Converter, qualPS)
import PureScript.Backend.Erl.Constants (erlExt)
import PureScript.Backend.Erl.Convert (Mode(..), codegenModule, initAcrossModules)
import PureScript.Backend.Erl.Convert.Common (erlModuleNamePs, erlModuleNameForeign)
import PureScript.Backend.Erl.Convert.Foreign (mkConverters)
import PureScript.Backend.Erl.Foreign (fullForeignSemantics)
import PureScript.Backend.Erl.Foreign.Analyze (Analyzer, analyzeCustom)
import PureScript.Backend.Erl.Parser (parseFile)
import PureScript.Backend.Erl.Printer as P
import PureScript.Backend.Optimizer.Builder (buildModules)
import PureScript.Backend.Optimizer.CoreFn (Module(..), ModuleName(..))
import PureScript.Backend.Optimizer.Directives (parseDirectiveFile)
import PureScript.Backend.Optimizer.Directives.Defaults (defaultDirectives)
import PureScript.Backend.Optimizer.Semantics.Foreign (ForeignSemantics)
import PureScript.Backend.Optimizer.Tracer.Printer (printModuleSteps)
import PureScript.CST.Errors (printParseError)
import Test.Utils (coreFnModulesFromOutput, errored, mkdirp)

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
main = customMain mempty

customMain :: CustomCodegen -> Effect Unit
customMain custom = do
  cliArgs <- Array.drop 2 <$> Process.argv
  case ArgParser.parseArgs "test" "" argParser cliArgs of
    Left err ->
      Console.error $ ArgParser.printArgError err
    Right args ->
      launchAff_ $ runCompileCustom custom args

moreDirectives :: String
moreDirectives = """
Stetson.HandlerProxy.provide arity=1
Stetson.HandlerProxy.accept arity=1
-- Allows us to optimize it specifically
Control.Applicative.when arity=3
"""


-- printer :: forall a. Dodo.Printer String a (Aff Unit)
-- printer = Printer
--   { emptyBuffer: ""
--   , writeText: \_ str buff -> buff <> str
--   , writeIndent: \_ str buff -> buff <> str
--   , writeBreak: \buff -> buff <> "\n"
--   , enterAnnotation: \_ _ buff -> buff
--   , leaveAnnotation: \_ _ buff -> buff
--   , flushBuffer: \buff -> buff
--   }

type CustomCodegen =
  { customAnalysis :: Array Analyzer
  , customEval :: Array ForeignSemantics
  , customCodegen :: Array Converter
  }

runCompile :: MainArgs -> Aff Unit
runCompile = runCompileCustom mempty

runCompileCustom :: CustomCodegen -> MainArgs -> Aff Unit
runCompileCustom custom { compile, filter, cwd } = do
  let
    customEval = fullForeignSemantics custom.customEval
    customCodegen = mkConverters custom.customCodegen
    customAnalysis = custom.customAnalysis
  liftEffect $ traverse_ Process.chdir cwd
  currentDir <- liftEffect Process.cwd
  let outputDir = Path.concat [ currentDir, "output-erl" ]
  let buildFile = Path.concat [ outputDir, "build.txt" ]
  mkdirp outputDir
  { coreFnModules, directives, finish, traceIdents } <- coreFnModulesFromOutput "output" filter >>= case _ of
    Left errors -> do
      for_ errors \(Tuple filePath err) -> do
        Console.error $ filePath <> " " <> err
      liftEffect $ Process.exit' 1
    Right (Tuple coreFnModules _) | List.null coreFnModules -> do
      Console.log "No corefn modules found in ./output; try building"
      liftEffect $ Process.exit' 0
    Right (Tuple _ Nothing) -> do
      Console.log "No corefn modules found in ./output; try building"
      liftEffect $ Process.exit' 0
    Right (Tuple coreFnModules (Just (Max timestamp))) -> do
      let moduleNames = Set.fromFoldable $ coreFnModules <#> \(Module { name: ModuleName name }) -> name
      customDirectives <- map (either mempty identity) $ try $ FS.readTextFile UTF8 $ Path.concat [ currentDir, "directives.txt" ]
      traceStrings <- map (either mempty identity) $ try $ FS.readTextFile UTF8 $ Path.concat [ currentDir, "traces.txt" ]
      when (traceStrings /= mempty) do
        FS.writeTextFile UTF8 "optimization-traces.txt" ""
      let
        traceIdents =
          Set.fromFoldable $ String.split (String.Pattern "\n") traceStrings
            # map (String.split (String.Pattern "#") >>> Array.head >>> fromMaybe "")
            # map String.trim
            # Array.filter (_ /= "")
            # map qualPS
      let allDirectives = defaultDirectives <> moreDirectives <> customDirectives
      let { directives } = parseDirectiveFile allDirectives
      do
        let { errors: directivesErrors } = parseDirectiveFile customDirectives
        when (not Array.null directivesErrors) do
          Console.warn "Warning: errors parsing ./directives.txt:"
          for_ directivesErrors \(Tuple lineContents { error, position: { line, column }}) -> do
            Console.warn $ fold
              [ "  Error at ./directives.txt:"
              , show (line+1)
              , ":"
              , show (column+1)
              , ":"
              , "\n    "
              , printParseError error
              , ":"
              , "\n      "
              , lineContents
              ]
      buildTimestamp <- try $ modifiedTimeMs <$> FS.stat buildFile
      case buildTimestamp of
        Right lastBuild | lastBuild >= timestamp -> do
          buildContents <- try $ FS.readTextFile UTF8 buildFile
          case buildContents of
            Right contents -> do
              case parseBuildFile contents of
                Just parsed
                  | String.trim parsed.allDirectives /= String.trim allDirectives -> do
                    Console.log "Rebuilding because directives changed"
                  | not Set.subset moduleNames parsed.moduleNames -> do
                    Console.log $ fold
                      [ "Rebuilding with "
                      , show (Set.size (Set.difference moduleNames parsed.moduleNames))
                      , " new/different module(s)"
                      ]
                  | otherwise -> do
                    checked <- try do
                      Console.log "Checking FFI files ..."
                      -- We need to check foreign files since their timestamps are
                      -- not tracked: we can just copy them if their interface
                      -- has not changed, otherwise we need to rebuild
                      for_ coreFnModules \(Module { name: ModuleName name, path: reportedPath }) -> do
                        let
                          -- Sorry, working around a weird language server bug
                          path = fromMaybe <*> String.stripPrefix (String.Pattern currentDir) $ reportedPath
                          moduleOutputDir = Path.concat [ outputDir, name ]
                          moduleOutputForeignPath = Path.concat [ moduleOutputDir, erlModuleNameForeign (ModuleName name) <> erlExt ]
                          fileForeign =
                            Path.concat [ currentDir, path ]
                              # (fromMaybe <*> String.stripSuffix (String.Pattern ".purs"))
                              # (_ <> ".erl")
                        foreignFile <- try $ FS.readTextFile UTF8 fileForeign
                        case foreignFile of
                          -- Check that the FFI file has the same interface
                          Right newContents -> do
                            -- No `try`, this fails back to upper `try` to cancel
                            -- if the file does not exist
                            previous <- FS.readTextFile UTF8 moduleOutputForeignPath
                            case parseFile previous, parseFile newContents of
                              Right prev, Right next | prev == next -> do
                                FS.writeTextFile UTF8 moduleOutputForeignPath newContents
                              _, _ -> do
                                throwError $ Aff.error $ "FFI interface changed " <> fileForeign
                          -- Check that the FFI file did not exist
                          -- (This requires that we delete FFI files from the
                          -- build dir when they disappear too!)
                          Left _ -> do
                            try (FS.stat moduleOutputForeignPath) >>=
                              case _ of
                                -- The file exists in build dir
                                Right _ -> throwError $ Aff.error $ "FFI file deleted " <> fileForeign
                                -- Both files do not exist: OK
                                Left _ -> pure unit
                        -- If all looks good, we fall into this
                      Console.log "... up to date!"
                      Console.log "Run `rm ./output-erl/build.txt` to force a rebuild"
                      liftEffect $ Process.exit' 0
                    case checked of
                      Right impossible -> absurd impossible
                      Left err -> do
                        Console.log $ "... " <> Aff.message err
                Nothing -> do
                  Console.warn "Could not parse ./output-erl/build.txt"
            _ -> pure unit -- Build file does not exist
        _ -> pure unit -- Build file does not exist
      FS.writeTextFile UTF8 buildFile $ printBuildFile { moduleNames: mempty, allDirectives }
      finish <- pure do
        FS.writeTextFile UTF8 buildFile $ printBuildFile { moduleNames, allDirectives }
      pure { coreFnModules, directives, finish, traceIdents }
  do
      erls <- liftEffect $ Ref.new []
      conventionsRef <- liftEffect $ Ref.new initAcrossModules
      coreFnModules # buildModules
        { directives
        , analyzeCustom: analyzeCustom custom.customAnalysis
        , foreignSemantics: customEval
        , traceIdents: traceIdents
        , onCodegenModule: \_ (Module { name: ModuleName name, path: reportedPath }) backend allSteps -> do
            -- Sorry, working around a weird language server bug
            let path = fromMaybe <*> String.stripPrefix (String.Pattern currentDir) $ reportedPath
            let moduleOutputDir = Path.concat [ outputDir, name ]
            let moduleOutputPath = Path.concat [ moduleOutputDir, erlModuleNamePs (ModuleName name) <> erlExt ]
            let moduleOutputForeignPath = Path.concat [ moduleOutputDir, erlModuleNameForeign (ModuleName name) <> erlExt ]
            let
              fileForeign =
                Path.concat [ currentDir, path ]
                  # (fromMaybe <*> String.stripSuffix (String.Pattern ".purs"))
                  # (_ <> ".erl")
            foreignFile <- try $ FS.readTextFile UTF8 fileForeign
            let
              foreignsE = case foreignFile of
                Left _ | Set.isEmpty backend.foreign -> Right mempty
                Left err -> Left $ "No foreigns file for " <> name <> " " <> Aff.message err
                Right content -> lmap parseErrorMessage $ parseFile content
            foreigns <- either (throwError <<< Aff.error) pure foreignsE
            prevConventions <- liftEffect $ Ref.read conventionsRef
            let
              Tuple codegened nextConventions =
                codegenModule { customEval, customCodegen, customAnalysis } NoDebug backend foreigns prevConventions
            let
              formatted =
                Dodo.print plainText Dodo.twoSpaces
                  $ P.printModule codegened
            liftEffect $ Ref.write nextConventions conventionsRef
            mkdirp moduleOutputDir
            FS.writeTextFile UTF8 moduleOutputPath formatted
            case foreignFile of
              Right contents -> do
                FS.writeTextFile UTF8 moduleOutputForeignPath contents
                liftEffect $ Ref.modify_ (_ <> [moduleOutputForeignPath]) erls
              Left _ -> do
                void $ try $ FS.rm' moduleOutputForeignPath
                  { force: false, maxRetries: 0, recursive: false, retryDelay: 10 }
            liftEffect $ Ref.modify_ (_ <> [moduleOutputPath]) erls
            unless (Array.null allSteps) do
              let allDoc = printModuleSteps (ModuleName name) allSteps <> Dodo.break <> Dodo.break
              FS.appendTextFile UTF8 "optimization-traces.txt" $ Dodo.print Dodo.plainText Dodo.twoSpaces allDoc
        , onPrepareModule: \build coreFnMod@(Module { name }) -> do
            let total = show build.moduleCount
            let index = show (build.moduleIndex + 1)
            let padding = power " " (SCU.length total - SCU.length index)
            Console.log $ "[" <> padding <> index <> " of " <> total <> "] Building " <> unwrap name
            pure coreFnMod
        }
      compiledFiles <- liftEffect $ Ref.read erls
      when compile do
        let ebin = Path.concat [ outputDir, "ebin" ]
        mkdirp ebin
        spawned <- execa "erlc" ([ "+no_ssa_opt", "-o", ebin, "-W0" ] <> compiledFiles) identity
        spawned.getResult >>= case _ of
          e@{ message } | errored e -> do
            Console.log $ withGraphics (foreground Red) "✗ failed to compile."
            Console.log message
          _ -> pure unit
      FS.writeTextFile UTF8 (Path.concat [ outputDir, "build_products.txt" ]) $
        String.joinWith "\n" compiledFiles <> "\n"
      -- Write out build file
      finish

-- | We maintain `./output-erl/build.txt` as a record of what modules have been
-- | built so we can avoid needless rebuilds. (Not a substitute for proper
-- | incrementalism.)
-- |
-- | The inputs to the build are the CoreFn modules and the set of directives,
-- | so we record them both, since `--filter` and `./directives.txt` can change.
-- | Additionally, the timestamp of the file is compared to the timestamps
-- | of the `corefn.json` files produced by `purs`.
type BuildFileInfo =
  { moduleNames :: Set String
  , allDirectives :: String
  }

-- Can bump this to invalidate the build file between versions
buildFileSep :: String
buildFileSep = "#### 0.0.3 ####\n"

printBuildFile :: BuildFileInfo -> String
printBuildFile { moduleNames, allDirectives } = fold
  [ moduleNames # foldMap \name -> name <> "\n"
  , buildFileSep
  , allDirectives
  ]

parseBuildFile :: String -> Maybe BuildFileInfo
parseBuildFile whole = do
  sepPos <- String.indexOf (String.Pattern buildFileSep) whole
  let { before, after } = String.splitAt sepPos whole
  allDirectives <- String.stripPrefix (String.Pattern buildFileSep) after
  let moduleNames = Set.fromFoldable $ String.split (String.Pattern "\n") $ String.trim before
  pure { moduleNames, allDirectives }

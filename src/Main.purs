module PureScript.Backend.Erl.Main where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Control.Alternative (guard)
import Control.Parallel (parTraverse)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, hush)
import Data.Foldable (fold, foldMap, foldl, for_, traverse_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (power)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), fst)
import Dodo (plainText)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_, throwError, try)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Library.Execa (execa)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Parsing (parseErrorMessage)
import PureScript.Backend.Erl.Calling (Converter, qualPS)
import PureScript.Backend.Erl.Constants (erlExt)
import PureScript.Backend.Erl.Convert (AcrossModules, Mode(..), codegenModule, initAcrossModules)
import PureScript.Backend.Erl.Convert.Common (erlModuleNamePs, erlModuleNameForeign)
import PureScript.Backend.Erl.Convert.Foreign (mkConverters)
import PureScript.Backend.Erl.Foreign (fullForeignSemantics)
import PureScript.Backend.Erl.Foreign.Analyze (Analyzer, analyzeCustom)
import PureScript.Backend.Erl.Parser (parseFile)
import PureScript.Backend.Erl.Printer as P
import PureScript.Backend.Optimizer.Builder (BuildState, buildModules)
import PureScript.Backend.Optimizer.CoreFn (Module(..), ModuleName(..), Qualified(..))
import PureScript.Backend.Optimizer.Directives (parseDirectiveFile)
import PureScript.Backend.Optimizer.Directives.Defaults (defaultDirectives)
import PureScript.Backend.Optimizer.QIMap as QIMap
import PureScript.Backend.Optimizer.Semantics (noDirectives, unionDirectives)
import PureScript.Backend.Optimizer.Semantics.Foreign (ForeignSemantics)
import PureScript.Backend.Optimizer.Tracer.Printer (printModuleSteps)
import PureScript.CST.Errors (printParseError)
import Test.Utils (coreFnModulesFromOutput, decideModules, errored, mkdirp)

type MainArgs =
  { compile :: Boolean
  , clean :: Boolean
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
    , clean:
        ArgParser.flag [ "--clean", "-C" ]
          "Run a full build, not an incremental one"
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
main = customMain stock

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
  , customDirectives :: String -- can be used to invalidate the build too
  , incrementalize :: Maybe Incrementalization
  }
type Incrementalization =
  { saveCacheToFile :: FilePath -> TotalBuildState -> Aff Unit
  , loadCacheFromFile :: FilePath -> Aff TotalBuildState
  , hashText :: String -> Aff Hash
  }
type Hash = String
stock =
  { customAnalysis: mempty
  , customEval: mempty
  , customCodegen: mempty
  , customDirectives: mempty
  , incrementalize: Nothing
  } :: CustomCodegen

-- Total build state: for `backend-optimizer` and for us, `backend-erl`
type TotalBuildState =
  { optimizerState :: BuildState
  , erlState :: AcrossModules
  }
-- Filter the build state relevant to one module
moduleBuildState :: ModuleName -> TotalBuildState -> TotalBuildState
moduleBuildState target { optimizerState, erlState } =
  { optimizerState:
    { built: Set.singleton target
    , directives: Tuple (fst optimizerState.directives # QIMap.matchModules (Set.singleton target)) Map.empty
    , implementations: optimizerState.implementations # QIMap.matchModules (Set.singleton target)
    }
  , erlState:
    { callingConventions: erlState.callingConventions # QIMap.matchModules (Set.singleton target)
    , constructors: erlState.constructors # Map.filterKeys case _ of
        Qualified (Just name) _ -> name == target
        _ -> false
    }
  }
-- Merge build states from several modules
mergeBuildState :: forall f. Traversable f => f TotalBuildState -> TotalBuildState
mergeBuildState thingies =
  { optimizerState:
    { built: foldMap _.optimizerState.built thingies
    , directives: foldl unionDirectives noDirectives $ _.optimizerState.directives <$> thingies
    , implementations: QIMap.unions $ _.optimizerState.implementations <$> thingies
    }
  , erlState:
    { callingConventions: QIMap.unions $ _.erlState.callingConventions <$> thingies
    , constructors: Map.unions $ _.erlState.constructors <$> thingies
    }
  }

runCompile :: MainArgs -> Aff Unit
runCompile = runCompileCustom stock

-- Check that a module is up to date, based on the current and cached hash of
-- corefn.json, and based on the FFI source and destination paths (it will
-- copy the FFI and mark it built if the FFI's interface has not changed)
checkModuleUpToDate ::
  { current :: Maybe Hash
  , cached :: Maybe Hash
  , ffi :: { from :: FilePath, to :: FilePath }
  } -> Aff Boolean
checkModuleUpToDate { current: Nothing } = Aff.throwError $ Aff.error "Missing hash"
checkModuleUpToDate { current: Just current, cached, ffi: { from, to } }
  | cached /= Just current = pure false
  | otherwise = do
    foreignFile <- try $ FS.readTextFile UTF8 from
    case foreignFile of
      -- Check that the FFI file has the same interface
      Right newContents -> do
        previous <- try $ FS.readTextFile UTF8 to
        case previous of
          Right oldContents ->
            case parseFile oldContents, parseFile newContents of
              Right prev, Right next | prev == next -> do
                FS.writeTextFile UTF8 to newContents
                pure true
              _, _ -> do
                pure false -- FFI interface changed
          Left _ -> pure false -- FFI file added
      -- Check that the FFI file did not exist
      -- (This requires that we delete FFI files from the
      -- build dir when they disappear too!)
      Left _ -> do
        try (FS.stat to) >>=
          case _ of
            -- The file exists in build dir
            Right _ -> pure false -- FFI file deleted
            -- Both files do not exist: OK
            Left _ -> pure true

runCompileCustom :: CustomCodegen -> MainArgs -> Aff Unit
runCompileCustom custom { compile, filter, cwd, clean } = do
  let
    customEval = fullForeignSemantics custom.customEval
    qustomEval = QIMap.fromMap customEval
    customCodegen = mkConverters custom.customCodegen
    customAnalysis = custom.customAnalysis
  liftEffect $ traverse_ Process.chdir cwd
  -- Important paths (not configurable yet)
  currentDir <- liftEffect Process.cwd
  let outputDir = Path.concat [ currentDir, "output-erl" ]
  let buildFile = Path.concat [ outputDir, "build.json" ]
  mkdirp outputDir

  -- Read identifiers to trace for debugging
  traceIdents <- do
    traceStrings <- map (either mempty identity) $ try $ FS.readTextFile UTF8 $ Path.concat [ currentDir, "traces.txt" ]
    when (traceStrings /= mempty) do
      FS.writeTextFile UTF8 "optimization-traces.txt" ""
    pure
      $ Set.fromFoldable $ String.split (String.Pattern "\n") traceStrings
      # map (String.split (String.Pattern "#") >>> Array.head >>> fromMaybe "")
      # map String.trim
      # Array.filter (_ /= "")
      # map qualPS

  -- Parse the directives: directives.txt and the default directives
  { directives, allDirectives } <- do
    userDirectives <- map (either mempty identity) $ try $ FS.readTextFile UTF8 $ Path.concat [ currentDir, "directives.txt" ]
    -- Report parse errors for `directives.txt` on its own
    let { errors: directivesErrors } = parseDirectiveFile userDirectives
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
    -- Concatenate and parse them all
    let
      allDirectives = String.joinWith "\n"
        [ defaultDirectives, moreDirectives, custom.customDirectives, userDirectives ]
      { directives } = parseDirectiveFile allDirectives
    pure { directives, allDirectives }

  -- Read in all of the corefn.json modules, and their hashes
  Console.log "Reading modules ..."
  { coreFnModules, moduleHashes: currentModuleHashes } <-
    coreFnModulesFromOutput "output" filter >>= case _ of
      Left errors -> do
        for_ errors \(Tuple filePath err) -> do
          Console.error $ filePath <> " " <> err
        liftEffect $ Process.exit' 1
      Right coreFnModules | List.null coreFnModules -> do
        Console.log "No corefn modules found in ./output; try building"
        liftEffect $ Process.exit' 0
      Right coreFnModulesWithSource -> do
        moduleHashList <- case custom.incrementalize of
          Just { hashText } -> coreFnModulesWithSource # parTraverse
            \{ name, corefn } -> Tuple name <$> hashText corefn
          Nothing -> mempty
        let moduleHashes = Map.fromFoldable moduleHashList
        -- Free these large strings
        -- TODO: free `full :: Lazy` somehow?
        let coreFnModules = map _ { corefn = "" } coreFnModulesWithSource
        pure { coreFnModules, moduleHashes }

  -- Check the status of each module: if we can skip building them for an incremental build
  { finish, unchanged } <- do
    buildContents <- try $ FS.readTextFile UTF8 buildFile
    unchanged <- case buildContents of
      Left _ -> pure Set.empty -- Build file does not exist
      Right contents -> do
        case parseBuildFile contents of
          Nothing -> do
            Console.warn "Could not parse ./output-erl/build.json"
            pure $ Set.empty
          Just parsed@{ moduleHashes: cachedModuleHashes }
            | String.trim parsed.allDirectives /= String.trim allDirectives -> do
              Console.log "Rebuilding everything because directives changed"
              pure $ Set.empty
            | otherwise -> do
              Console.log "Checking files ..."
              -- We need to check foreign files since their timestamps are
              -- not tracked: we can just copy them if their interface
              -- has not changed, otherwise we need to rebuild
              fold <$> flip parTraverse coreFnModules \{ name: ModuleName name, path: reportedPath } -> do
                let
                  -- Sorry, working around a weird language server bug/inconsistency
                  path = fromMaybe <*> String.stripPrefix (String.Pattern currentDir) $ reportedPath
                  moduleOutputDir = Path.concat [ outputDir, name ]
                  moduleOutputForeignPath = Path.concat [ moduleOutputDir, erlModuleNameForeign (ModuleName name) <> erlExt ]
                  fileForeign =
                    Path.concat [ currentDir, path ]
                      # (fromMaybe <*> String.stripSuffix (String.Pattern ".purs"))
                      # (_ <> ".erl")
                upToDate <- checkModuleUpToDate
                  { current: Map.lookup (ModuleName name) currentModuleHashes
                  , cached: Map.lookup name cachedModuleHashes
                  , ffi: { from: fileForeign, to: moduleOutputForeignPath }
                  }
                pure if upToDate then Set.singleton (ModuleName name) else mempty
    -- Check if everything was built in order to exit early
    when (Set.size unchanged == List.length coreFnModules) do
      Console.log "... up to date!"
      Console.log "Run `rm ./output-erl/build.json` to force a rebuild"
      liftEffect $ Process.exit' 0
    -- Write out an *empty* build result file, in case it is canceled
    -- FS.writeTextFile UTF8 buildFile $ printBuildFile { moduleHashes: mempty, allDirectives, version: buildVersion }
    -- And return the function to write out the completed build result file
    finish <- pure do
      moduleHashes <- pure $ Map.fromFoldable $ currentModuleHashes # mapWithIndex
        \(ModuleName k) v -> Tuple k v
      FS.writeTextFile UTF8 buildFile $ printBuildFile { moduleHashes, allDirectives, version: buildVersion }
    pure { finish, unchanged }

  -- Modules can be:
  -- - In need of rebuild: hashes don't match, or downstream of a rebuild
  -- - An immediate or transitive dependency of a rebuild: load from cache
  -- - Inert, not directly related to rebuildable modules
  let { toBeBuilt, pleaseLoadCache } = decideModules { scanned: coreFnModules, unchanged }

  -- Restore the incremental state for modules in `pleaseLoadCache`
  incremental <- case custom.incrementalize of
    _ | clean || Set.isEmpty pleaseLoadCache -> pure Nothing
    Nothing -> pure Nothing
    Just incrementalize -> do
      Console.log $ "Reading incremental state (" <> show (Set.size pleaseLoadCache) <> " files) ..."
      map hush $ try $ Aff.sequential do
        mergeBuildState <$> forWithIndex (Set.toMap pleaseLoadCache)
          \(ModuleName name) _ -> Aff.parallel do
            incrementalize.loadCacheFromFile (Path.concat [ outputDir, name, "buildcache.gz" ])

  -- Now we can get on with the actual build!
  { optimizerState: _optimizerState, erlState: _erlState, compiledFiles } <- do
    -- List of all output Erlang modules (PureScript and FFI)
    erls <- liftEffect $ Ref.new []
    -- Ref of *our* build state for backend-erl
    erlStateRef <- liftEffect $ Ref.new $ case incremental of
      Nothing -> initAcrossModules
      Just { erlState } -> erlState
    asyncActions <- liftEffect $ Ref.new []
    let
      monitor :: Aff Unit -> Aff Unit
      monitor = liftEffect <<< do
        launchAff >=> \fiber -> Ref.modify_ (_ <> [fiber]) asyncActions
    -- Call the optimizer and let it do its thing
    optimizerState <- toBeBuilt # buildModules
      { directives
      , analyzeCustom: analyzeCustom custom.customAnalysis
      , foreignSemantics: customEval
      , traceIdents: traceIdents
      , incremental: incremental <#> _.optimizerState
      , onCodegenModule: \buildEnv (Module { name: ModuleName name, path: reportedPath }) backend allSteps -> do
          -- Sorry, working around a weird language server bug/inconsistency
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
          prevConventions <- liftEffect $ Ref.read erlStateRef
          let
            Tuple codegened nextConventions =
              codegenModule { customEval: qustomEval, customCodegen, customAnalysis } NoDebug backend foreigns prevConventions
          let
            formatted =
              Dodo.print plainText Dodo.twoSpaces
                $ P.printModule codegened
          liftEffect $ Ref.write nextConventions erlStateRef
          mkdirp moduleOutputDir
          unless (Array.null allSteps) do
            let allDoc = printModuleSteps (ModuleName name) allSteps <> Dodo.break <> Dodo.break
            FS.appendTextFile UTF8 "optimization-traces.txt" $ Dodo.print Dodo.plainText Dodo.twoSpaces allDoc
          -- Actions that can happen async
          monitor do
            FS.writeTextFile UTF8 moduleOutputPath formatted
            case foreignFile of
              Right contents -> do
                FS.writeTextFile UTF8 moduleOutputForeignPath contents
                liftEffect $ Ref.modify_ (_ <> [moduleOutputForeignPath]) erls
              Left _ -> do
                void $ try $ FS.rm' moduleOutputForeignPath
                  { force: false, maxRetries: 0, recursive: false, retryDelay: 10 }
            liftEffect $ Ref.modify_ (_ <> [moduleOutputPath]) erls
            let optimizerState = { built: buildEnv.built, implementations: buildEnv.implementations, directives: buildEnv.directives }
            let erlState = nextConventions
            case custom.incrementalize of
              Nothing -> pure unit
              Just { saveCacheToFile } -> do
                saveCacheToFile (Path.concat [ outputDir, name, "buildcache.gz" ]) $
                  moduleBuildState (ModuleName name) { optimizerState, erlState }
      , onPrepareModule: \build coreFnMod@(Module { name }) -> do
          let total = show build.moduleCount
          let index = show (build.moduleIndex + 1)
          let padding = power " " (SCU.length total - SCU.length index)
          Console.log $ "[" <> padding <> index <> " of " <> total <> "] Building " <> unwrap name
          pure coreFnMod
      }
    -- Wait for all of the async actions
    liftEffect (Ref.read asyncActions) >>= traverse_ Aff.joinFiber
    compiledFiles <- liftEffect $ Ref.read erls
    erlState <- liftEffect $ Ref.read erlStateRef
    pure { optimizerState, erlState, compiledFiles }

  -- Final tasks:
  do
    -- Write out the build information file and the build products file
    finish
    FS.writeTextFile UTF8 (Path.concat [ outputDir, "build_products.txt" ]) $
      String.joinWith "\n" compiledFiles <> "\n"
  do
    -- And optionally compile them all with `erlc` immediately
    when compile do
      let ebin = Path.concat [ outputDir, "ebin" ]
      mkdirp ebin
      spawned <- execa "erlc" ([ "+no_ssa_opt", "-o", ebin, "-W0" ] <> compiledFiles) identity
      spawned.getResult >>= case _ of
        e@{ message } | errored e -> do
          Console.log $ withGraphics (foreground Red) "✗ failed to compile."
          Console.log message
        _ -> pure unit

-- | We maintain `./output-erl/build.json` as a record of what modules have been
-- | built so we can perform incremental builds.
-- |
-- | The inputs to the build are the CoreFn modules and the set of directives,
-- | so we record them both, since `--filter` and `./directives.txt` can change.
-- | The CoreFn modules all end up with similar timestamps, so we have to record
-- | their hashes and always compare those. (We are reading the text of the
-- | file anyways.)
type BuildFileInfo =
  { moduleHashes :: Map String String
  , allDirectives :: String
  , version :: String
  }
-- Can bump this to invalidate the build file between versions
buildVersion = "0.0.4" :: String

printBuildFile :: BuildFileInfo -> String
printBuildFile = Json.encodeJson >>> Json.stringifyWithIndent 2

parseBuildFile :: String -> Maybe BuildFileInfo
parseBuildFile contents = do
  parsed <- hush $ Json.decodeJson =<< Json.parseJson contents
  guard $ parsed.version == buildVersion
  pure parsed

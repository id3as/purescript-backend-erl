-- A majority of the code below was copied from
-- https://github.com/aristanetworks/purescript-backend-optimizer/blob/main/backend-es/test/Main.purs
-- To fullfill copyright requirements...
--    Copyright © 2022 Arista Networks, Inc.
--    MIT license: https://opensource.org/license/mit/
module Test.Main where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Control.Alternative (guard, (<|>))
import Data.Array (findMap)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..), either)
import Data.Foldable (elem, for_)
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid (power)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Dodo (plainText)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_, throwError, try)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Glob.Basic (expandGlobs)
import Node.Library.Execa (execa)
import Node.Path as Path
import Node.Process as Process
import Parsing (parseErrorMessage)
import PureScript.Backend.Erl.Constants (erlExt)
import PureScript.Backend.Erl.Convert (codegenModule, initAcrossModules)
import PureScript.Backend.Erl.Convert.Common (erlModuleNamePs, erlModuleNameForeign)
import PureScript.Backend.Erl.Foreign (fullForeignSemantics)
import PureScript.Backend.Erl.Foreign.Analyze (analyzeCustom)
import PureScript.Backend.Erl.Parser (parseFile)
import PureScript.Backend.Erl.Printer as P
import PureScript.Backend.Optimizer.Builder (buildModules)
import PureScript.Backend.Optimizer.Convert (BackendModule)
import PureScript.Backend.Optimizer.CoreFn (Comment(..), Ident(..), Module(..), ModuleName(..))
import PureScript.Backend.Optimizer.Directives (parseDirectiveFile)
import PureScript.Backend.Optimizer.Directives.Defaults (defaultDirectives)
import Test.Utils (bufferToUTF8, coreFnModulesFromOutput, errored, execWithStdin, loadModuleMain, mkdirp, rmrf, spawnFromParent)

type TestArgs =
  { accept :: Boolean
  , compile :: Boolean
  , run :: Boolean
  , filter :: NonEmptyArray String
  }

argParser :: ArgParser TestArgs
argParser =
  ArgParser.fromRecord
    { accept:
        ArgParser.flag [ "--accept", "-a" ]
          "Accepts snapshot output"
          # ArgParser.boolean
          # ArgParser.default false
    , compile:
        ArgParser.flag [ "--compile", "-c" ]
          "Compile generated Erlang with erlc"
          # ArgParser.boolean
          # ArgParser.default false
    , run:
        ArgParser.flag [ "--run", "-r" ]
          "Run generated Erlang main functions with erl (implies --compile)"
          # ArgParser.boolean
          # ArgParser.default false
    , filter:
        ArgParser.argument [ "--filter", "-f" ]
          "Filter tests matching a prefix"
          # ArgParser.unfolded1
          # ArgParser.default (pure "Snapshot.*")
    }

main :: Effect Unit
main = do
  cliArgs <- Array.drop 2 <$> Process.argv
  case ArgParser.parseArgs "test" "" argParser cliArgs of
    Left err ->
      Console.error $ ArgParser.printArgError err
    Right args ->
      launchAff_ $ runSnapshotTests args { compile = args.compile || args.run }

runSnapshotTests :: TestArgs -> Aff Unit
runSnapshotTests { accept, compile, run, filter } = do
  Console.log "Hi"
  liftEffect $ Process.chdir $ Path.concat [ "test-snapshots" ]
  spawnFromParent "spago" [ "build" ]
  Console.log "Did spago"
  snapshotDir <- liftEffect Process.cwd
  snapshotPaths <- expandGlobs (Path.concat [ snapshotDir, "src", "snapshots-input" ])
    [ "Snapshot.*.purs" ]
  Console.log "Did globs"
  outputRef <- liftEffect $ Ref.new []
  let snapshotsOut = Path.concat [ snapshotDir, "src", "snapshots-output" ]
  let testOut = Path.concat [ snapshotDir, "test-out" ]
  let ebin = Path.concat [ testOut, "ebin" ]
  mkdirp snapshotsOut
  rmrf testOut
  mkdirp testOut
  mkdirp ebin
  conventionsRef <- liftEffect $ Ref.new initAcrossModules
  Console.log "Did setup"
  coreFnModulesFromOutput "output" filter >>= case _ of
    Left errors -> do
      for_ errors \(Tuple filePath err) -> do
        Console.error $ filePath <> " " <> err
      liftEffect $ Process.exit' 1
    Right coreFnModules -> do
      Console.log "Got modules"
      let { directives } = parseDirectiveFile defaultDirectives
      coreFnModules # buildModules
        { directives
        , analyzeCustom
        , foreignSemantics: fullForeignSemantics
        , traceIdents: mempty
        , onCodegenModule: \_ (Module { name: ModuleName name, path: reportedPath, exports }) (backend) _ -> do
            -- Sorry, working around a weird language server bug
            let path = fromMaybe <*> String.stripPrefix (String.Pattern snapshotDir) $ reportedPath
            let testFileDir = Path.concat [ testOut, name ]
            let testFilePath = Path.concat [ testFileDir, erlModuleNamePs (ModuleName name) <> erlExt ]
            let testFileForeignPath = Path.concat [ testFileDir, erlModuleNameForeign (ModuleName name) <> erlExt ]
            let snapshotDirFile = Path.concat [ snapshotDir, path ]
            let
              snapshotDirFileForeign =
                Path.concat [ snapshotDir, path ]
                  # (fromMaybe <*> String.stripSuffix (String.Pattern ".purs"))
                  # (_ <> ".erl")
            foreignFile <- try $ FS.readTextFile UTF8 snapshotDirFileForeign
            let foreignsE = either (Right <<< mempty) parseFile foreignFile
            foreigns <- either (throwError <<< Aff.error <<< parseErrorMessage) pure foreignsE
            prevConventions <- liftEffect $ Ref.read conventionsRef
            let
              Tuple codegened nextConventions = codegenModule backend foreigns prevConventions
              formatted =
                Dodo.print plainText Dodo.twoSpaces
                  $ P.printModule codegened
            liftEffect $ Ref.write nextConventions conventionsRef
            -- liftEffect $ debugModule backend  -- show backend
            mkdirp testFileDir
            FS.writeTextFile UTF8 testFilePath formatted
            r <- _.getResult =<< execa "cp" [ snapshotDirFileForeign, testFileForeignPath ] identity
            case r of
              _ | not errored r, run -> do
                _ <- loadModuleMain
                  { ebin
                  , modulePath: testFileForeignPath
                  , runMain: Nothing
                  }
                pure unit
              _ -> pure unit
            if Set.member snapshotDirFile snapshotPaths then do
              -- originalFileSourceCode <- FS.readTextFile UTF8 snapshotDirFile
              let
                hasMain =
                  Nothing <$ guard (Ident "main" `elem` exports)
                    <|> Just <$> hasExpected backend
              liftEffect $ Ref.modify_
                (_ <> [Tuple name ({ formatted, failsWith: hasFails backend, hasMain })])
                outputRef
            else when run do
              result <- loadModuleMain
                { runMain: Nothing
                , modulePath: testFilePath
                , ebin
                }
              case result of
                Left { message } -> do
                  Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " failed to compile."
                  Console.log message
                Right _ -> do
                  pure unit
        , onPrepareModule: \build coreFnMod@(Module { name }) -> do
            let total = show build.moduleCount
            let index = show (build.moduleIndex + 1)
            let padding = power " " (SCU.length total - SCU.length index)
            Console.log $ "[" <> padding <> index <> " of " <> total <> "] Building " <> unwrap name
            pure coreFnMod
        }
      outputModules <- liftEffect $ Ref.read outputRef
      results <- for outputModules \(Tuple name { formatted, failsWith, hasMain }) -> do
        let
          snapshotFilePath = Path.concat [ snapshotsOut, name <> erlExt ]
          runAcceptedTest
            | compile = do
              let mod = erlModuleNamePs (ModuleName name)
              erlangFile <- liftEffect $ Path.resolve [ testOut, name ] $ mod <> erlExt
              erlangScript <- liftEffect $ Path.resolve [ ebin ] $ mod <> ".escript"
              result <- loadModuleMain
                { runMain: { scriptFile: erlangScript, moduleName: name, expected: _ } <$> hasMain <* guard run
                , modulePath: erlangFile
                , ebin
                }
              case result of
                Left { message }
                  | matchesFail message failsWith ->
                      pure true
                  | otherwise -> do
                      Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " failed."
                      Console.log message
                      pure false
                Right _
                  | isJust failsWith -> do
                      Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <>
                        " succeeded when it should have failed."
                      pure false
                  | otherwise ->
                      pure true
            | otherwise = pure true
        attempt (FS.readTextFile UTF8 snapshotFilePath) >>= case _ of
          Left _ -> do
            Console.log $ withGraphics (foreground Yellow) "✓" <> " " <> name <> " saved."
            FS.writeTextFile UTF8 snapshotFilePath formatted
            pure true
          Right prevOutput
            | formatted == prevOutput ->
                runAcceptedTest
            | accept -> do
                Console.log $ withGraphics (foreground Yellow) "✓" <> " " <> name <> " accepted."
                FS.writeTextFile UTF8 snapshotFilePath formatted
                runAcceptedTest
            | otherwise -> do
                Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " failed."
                diff <- bufferToUTF8 <<< _.stdout =<< execWithStdin
                  ("diff " <> snapshotFilePath <> " -")
                  formatted
                Console.log diff
                pure false
      unless (Foldable.and results) do
        liftEffect $ Process.exit' 1
      pure unit

hasFails :: BackendModule -> Maybe String
hasFails = findMap go <<< _.comments
  where
  go = case _ of
    LineComment comm ->
      String.stripPrefix (Pattern "@fails ") (String.trim comm)
    _ ->
      Nothing

hasExpected :: BackendModule -> Maybe String
hasExpected = findMap go <<< _.comments
  where
  go = case _ of
    LineComment comm ->
      String.stripPrefix (Pattern "@expected ") (String.trim comm)
    _ ->
      Nothing

matchesFail :: String -> Maybe String -> Boolean
matchesFail errMsg = case _ of
  Just msg ->
    String.contains (Pattern msg) errMsg
  Nothing ->
    false

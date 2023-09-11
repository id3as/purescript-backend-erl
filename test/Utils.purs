-- A majority of the code below was copied from
-- https://github.com/aristanetworks/purescript-backend-optimizer/blob/main/backend-es/test/Utils.purs
-- https://github.com/aristanetworks/purescript-backend-optimizer/blob/main/backend-es/src/Main.purs
--
-- To fullfill copyright requirements...
--    Copyright © 2022 Arista Networks, Inc.
--    MIT license: https://opensource.org/license/mit/
module Test.Utils where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Except (ExceptT(..), lift, runExceptT)
import Control.Parallel (parTraverse)
import Data.Argonaut as Json
import Data.Array (intercalate)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Bifunctor (lmap)
import Data.Compactable (separate)
import Data.Either (Either(..))
import Data.Foldable (foldl, for_)
import Data.Lazy as Lazy
import Data.List (List)
import Data.Maybe (Maybe(..), maybe)
import Data.Posix.Signal (Signal(..))
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, effectCanceler, error, makeAff, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.Object as FO
import Node.Buffer (Buffer, freeze)
import Node.Buffer.Immutable as ImmutableBuffer
import Node.ChildProcess (ExecResult, Exit(..), defaultExecOptions, defaultSpawnOptions, inherit)
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.FS.Perms (mkPerms)
import Node.FS.Perms as Perms
import Node.FS.Stats as Stats
import Node.FS.Stream (createReadStream, createWriteStream)
import Node.Glob.Basic (expandGlobs)
import Node.Library.Execa (ExecaError, ExecaSuccess, execa)
import Node.Path (FilePath)
import Node.Process as Process
import Node.Stream as Stream
import PureScript.Backend.Erl.Convert (erlModuleNamePs)
import PureScript.Backend.Optimizer.CoreFn (Ann, Module, ModuleName(..))
import PureScript.Backend.Optimizer.CoreFn.Json (decodeModule)
import PureScript.Backend.Optimizer.CoreFn.Sort (emptyPull, pullResult, resumePull, sortModules)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Types as CSTT

spawnFromParent :: String -> Array String -> Aff Unit
spawnFromParent command args = makeAff \k -> do
  childProc <- ChildProcess.spawn command args defaultSpawnOptions { stdio = inherit }
  ChildProcess.onExit childProc case _ of
    Normally code
      | code > 0 -> Process.exit code
      | otherwise -> k (Right unit)
    BySignal _ ->
      Process.exit 1
  pure $ effectCanceler do
    ChildProcess.kill SIGABRT childProc

execWithStdin :: String -> String -> Aff ExecResult
execWithStdin command input = makeAff \k -> do
  childProc <- ChildProcess.exec command defaultExecOptions (k <<< pure)
  _ <- Stream.writeString (ChildProcess.stdin childProc) UTF8 input mempty
  Stream.end (ChildProcess.stdin childProc) mempty
  pure $ effectCanceler $ ChildProcess.kill SIGABRT childProc

bufferToUTF8 :: Buffer -> Aff String
bufferToUTF8 = liftEffect <<< map (ImmutableBuffer.toString UTF8) <<< freeze

mkdirp :: FilePath -> Aff Unit
mkdirp path = FS.mkdir' path { recursive: true, mode: mkPerms Perms.all Perms.all Perms.all }

rmrf :: FilePath -> Aff Unit
rmrf path = FS.rm' path { recursive: true, force: true, maxRetries: 0, retryDelay: 0 }

cpr :: FilePath -> FilePath -> Aff Unit
cpr from to = do
  spawned <- execa "cp" [ "-r", from, to ] identity
  spawned.result >>= case _ of
    Left e ->
      Console.error e.message
    Right _ ->
      pure unit

loadModuleMain
  :: { runMain :: Maybe
       { scriptFile :: String
       , moduleName :: String
       , expected :: Maybe String
       }
     , modulePath :: String
     , ebin :: String
     }
  -> Aff (Either ExecaError ExecaSuccess)
loadModuleMain { modulePath, ebin, runMain } = do
  spawned1 <- execa "erlc" [ "-W0", modulePath ]
    _ { cwd = Just ebin }
  spawned1.result >>= case _, runMain of
    Left e, _ -> pure (Left e)
    Right r, Nothing -> pure (Right r)
    Right _, Just { scriptFile, moduleName, expected } -> do
      let mod = ModuleName moduleName
      let init x = "(" <> erlModuleNamePs mod <> ":" <> x <> "())"
      FS.writeTextFile UTF8 scriptFile $ intercalate "\n"
        [ "#!/usr/bin/env escript"
        , case expected of
            Nothing -> "main(_) -> (" <> init "main" <> ")()."
            Just value ->
              "main(_) -> assertEq(" <> init "result" <> ", (" <> value <> "))." <>
              """
              assertEq(X, Y) when X =:= Y -> unit;
              assertEq(X, Y) -> erlang:error({{actual, X}, {expected, Y}}).
              """
        ]
      -- Work around execa bug: https://github.com/JordanMartinez/purescript-node-execa/pull/16
      e <- liftEffect Process.getEnv
      spawned2 <- execa "escript" [ scriptFile ] _ { env = Just (FO.union (FO.singleton "ERL_FLAGS" $ "-pa " <> ebin) e), extendEnv = Just true }
      spawned2.result

copyFile :: FilePath -> FilePath -> Aff Unit
copyFile from to = do
  stats <- FS.stat from
  unless (Stats.isFile stats) do
    throwError $ error $ "Not a file: " <> from
  makeAff \k -> do
    src <- createReadStream from
    dst <- createWriteStream to
    res <- Stream.pipe src dst
    Stream.onError src (k <<< Left)
    Stream.onError dst (k <<< Left)
    Stream.onError res (k <<< Left)
    Stream.onFinish res (k (Right unit))
    pure $ effectCanceler do
      Stream.destroy res
      Stream.destroy dst
      Stream.destroy src

coreFnModulesFromOutput
  :: String
  -> NonEmptyArray String
  -> Aff (Either (NonEmptyArray (Tuple FilePath String)) (List (Module Ann)))
coreFnModulesFromOutput path globs = runExceptT do
  paths <- Set.toUnfoldable <$> lift
    (expandGlobs path ((_ <> "/corefn.json") <$> NonEmptyArray.toArray globs))
  case NonEmptyArray.toArray globs of
    [ "**" ] ->
      sortModules <$> modulesFromPaths paths
    _ ->
      go <<< foldl resumePull emptyPull =<< modulesFromPaths paths
  where
  modulesFromPaths paths = ExceptT do
    { left, right } <- separate <$> parTraverse readCoreFnModule paths
    pure $ maybe (Right right) Left $ NonEmptyArray.fromArray left

  pathFromModuleName (ModuleName mn) =
    path <> "/" <> mn <> "/corefn.json"

  go pull = case pullResult pull of
    Left needed ->
      go <<< foldl resumePull pull =<< modulesFromPaths
        (pathFromModuleName <$> NonEmptySet.toUnfoldable needed)
    Right modules ->
      pure $ Lazy.force modules

readCoreFnModule :: String -> Aff (Either (Tuple FilePath String) (Module Ann))
readCoreFnModule filePath = do
  contents <- FS.readTextFile UTF8 filePath
  case lmap Json.printJsonDecodeError <<< decodeModule =<< Json.jsonParser contents of
    Left err -> do
      pure $ Left $ Tuple filePath err
    Right mod ->
      pure $ Right mod

-- | Returns `Right true` if the source code has this type signature
-- | somewhere in it:
-- | ```
-- | main :: Effect Unit
-- | ```
-- |
-- | If `Effect` or `Unit` are qualified by a module alias,
-- | this will not return `true`.
-- | ```
-- | main :: Effect.Effect Prelude.Unit
-- | ```
canRunMain :: String -> Either String Boolean
canRunMain sourceCode =
  case parseModule sourceCode of
    ParseSucceeded (CSTT.Module { body: CSTT.ModuleBody { decls } }) ->
      pure $ Array.any isMain decls
    ParseSucceededWithErrors _ errs ->
      Left $ Array.intercalate "\n"
        [ "canRunMain - could not completely parse file."
        , Array.intercalate "\n" $ map printPositionedError $ NonEmptyArray.toArray errs
        ]
    ParseFailed err ->
      Left $ Array.intercalate "\n"
        [ "canRunMain - could not parse file."
        , printPositionedError err
        ]
  where
  printPositionedError err = Array.intercalate "\n"
    [ ""
    , "Position: " <> show err.position
    , "Reason: " <> printParseError err.error
    ]
  isMain = case _ of
    CSTT.DeclSignature
      ( CSTT.Labeled
          { label: CSTT.Name { name: CSTT.Ident "main" }
          , value:
              CSTT.TypeApp
                (CSTT.TypeConstructor (CSTT.QualifiedName { name: CSTT.Proper "Effect" }))
                ( NonEmptyArray
                    [ CSTT.TypeConstructor (CSTT.QualifiedName { name: CSTT.Proper "Unit" })
                    ]
                )
          }
      ) -> true
    _ -> false

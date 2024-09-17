-- A majority of the code below was copied from
-- https://github.com/aristanetworks/purescript-backend-optimizer/blob/main/backend-es/test/Utils.purs
-- https://github.com/aristanetworks/purescript-backend-optimizer/blob/main/backend-es/src/Main.purs
--
-- To fullfill copyright requirements...
--    Copyright © 2022 Arista Networks, Inc.
--    MIT license: https://opensource.org/license/mit/
module Test.Utils where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Writer (WriterT(..), runWriterT)
import Control.Parallel (parTraverse)
import Data.Argonaut as Json
import Data.Array (intercalate)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.Compactable (separate)
import Data.Either (Either(..))
import Data.Foldable (foldMap, foldl)
import Data.Lazy as Lazy
import Data.List (List)
import Data.Maybe (Maybe(..), maybe)
import Data.Ord.Max (Max(..))
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.String as String
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff, Milliseconds, effectCanceler, error, makeAff, throwError)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Class.Console as Console
import Foreign.Object as FO
import Node.Buffer (Buffer, freeze)
import Node.Buffer.Immutable as ImmutableBuffer
import Node.ChildProcess (ExecResult)
import Node.ChildProcess as ChildProcess
import Node.ChildProcess.Types (Exit(..))
import Node.Encoding (Encoding(..))
import Node.EventEmitter as EE
import Node.FS.Aff as FS
import Node.FS.Perms (mkPerms)
import Node.FS.Perms as Perms
import Node.FS.Stats (modifiedTimeMs)
import Node.FS.Stats as Stats
import Node.FS.Stream (createReadStream, createWriteStream)
import Node.Glob.Basic (expandGlobs)
import Node.Library.Execa (ExecaResult, execa)
import Node.Path (FilePath)
import Node.Process as Process
import Node.Stream as Stream
import PureScript.Backend.Erl.Convert.Common (erlModuleNamePs)
import PureScript.Backend.Optimizer.CoreFn (Ann, Module, ModuleName(..))
import PureScript.Backend.Optimizer.CoreFn.Json (decodeModule)
import PureScript.Backend.Optimizer.CoreFn.Sort (emptyPull, pullResult, resumePull, sortModules)

spawnFromParent :: String -> Array String -> Aff Unit
spawnFromParent command args = makeAff \k -> do
  childProc <- ChildProcess.spawn command args
  childProc # EE.on_ ChildProcess.exitH case _ of
    Normally code
      | code > 0 -> Process.exit' code
      | otherwise -> k (Right unit)
    BySignal _ ->
      Process.exit' 1
  pure $ effectCanceler do
    void $ ChildProcess.kill childProc

execWithStdin :: String -> String -> Aff ExecResult
execWithStdin command input = makeAff \k -> do
  childProc <- ChildProcess.exec' command identity (k <<< pure)
  _ <- Stream.writeString (ChildProcess.stdin childProc) UTF8 input
  Stream.end (ChildProcess.stdin childProc)
  pure $ effectCanceler $ void $ ChildProcess.kill childProc

bufferToUTF8 :: Buffer -> Aff String
bufferToUTF8 = liftEffect <<< map (ImmutableBuffer.toString UTF8) <<< freeze

mkdirp :: FilePath -> Aff Unit
mkdirp path = FS.mkdir' path { recursive: true, mode: mkPerms Perms.all Perms.all Perms.all }

rmrf :: FilePath -> Aff Unit
rmrf path = FS.rm' path { recursive: true, force: true, maxRetries: 0, retryDelay: 0 }

cpr :: FilePath -> FilePath -> Aff Unit
cpr from to = do
  spawned <- execa "cp" [ "-r", from, to ] identity
  spawned.getResult >>= case _ of
    e | errored e ->
      Console.error e.message
    _ ->
      pure unit

errored :: forall r. { exit :: Exit | r } -> Boolean
errored { exit: Normally 0 } = false
errored _ = true

loadModuleMain
  :: { runMain :: Maybe
       { scriptFile :: String
       , moduleName :: String
       , expected :: Maybe String
       }
     , modulePath :: String
     , ebin :: String
     }
  -> Aff (Either ExecaResult ExecaResult)
loadModuleMain { modulePath, ebin, runMain } = do
  -- Console.log $ "erlc " <> modulePath
  spawned1 <- execa "erlc" [ "+no_ssa_opt", "-o", ebin, modulePath ] identity
  spawned1.getResult >>= case _, runMain of
    e, _ | errored e -> pure (Left e)
    r, Nothing -> do
      when (not String.null r.stdout) do
        log r.stdout
      pure (Right r)
    _, Just { scriptFile, moduleName, expected } -> do
      let mod = ModuleName moduleName
      let init x = "(" <> erlModuleNamePs mod <> ":" <> x <> "())"
      -- Console.log $ "run " <> modulePath
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
      -- Console.log $ "escript " <> scriptFile
      spawned2 <- execa "escript" [ scriptFile ] _ { env = Just ((FO.singleton "ERL_FLAGS" $ "-pa " <> ebin)), extendEnv = Just true }
      spawned2.getResult >>= case _ of
        e | errored e -> pure (Left e)
        r -> pure (Right r)

copyFile :: FilePath -> FilePath -> Aff Unit
copyFile from to = do
  stats <- FS.stat from
  unless (Stats.isFile stats) do
    throwError $ error $ "Not a file: " <> from
  makeAff \k -> do
    src <- createReadStream from
    dst <- createWriteStream to
    Stream.pipe src dst
    EE.on_ Stream.errorH (k <<< Left) src
    EE.on_ Stream.errorH (k <<< Left) dst
    EE.on_ Stream.finishH (k (Right unit)) dst
    pure $ effectCanceler do
      Stream.destroy dst
      Stream.destroy src

type LastTimestamp = Maybe (Max Milliseconds)

-- | Read `corefn.json` files and return the latest timestamp. Unlike `purs`
-- | itself, which tracks timestamps exactly because source control systems
-- | may revert to older timestamps and still require a rebuild, we can trust
-- | the timestamps of `corefn.json` files since they are managed by the
-- | compiler (`purs compile` and `purs ide`).
coreFnModulesFromOutput
  :: String
  -> NonEmptyArray String
  -> Aff (Either (NonEmptyArray (Tuple FilePath String)) (Tuple (List (Module Ann)) LastTimestamp))
coreFnModulesFromOutput path globs = runExceptT $ runWriterT do
  paths <- Set.toUnfoldable <$> liftAff
    (expandGlobs path ((_ <> "/corefn.json") <$> NonEmptyArray.toArray globs))
  case NonEmptyArray.toArray globs of
    [ "**" ] ->
      sortModules <$> modulesFromPaths paths
    _ ->
      go <<< foldl resumePull emptyPull =<< modulesFromPaths paths
  where
  modulesFromPaths :: _ -> WriterT LastTimestamp (ExceptT (NonEmptyArray (Tuple FilePath String)) Aff) _
  modulesFromPaths paths = WriterT $ ExceptT do
    { left, right } <- separate <$> parTraverse readCoreFnModule paths
    case NonEmptyArray.fromArray left of
      Nothing -> pure $ Right $ Tuple (map fst right) (foldMap snd right)
      Just errors -> pure $ Left errors

  pathFromModuleName (ModuleName mn) =
    path <> "/" <> mn <> "/corefn.json"

  go pull = case pullResult pull of
    Left needed ->
      go <<< foldl resumePull pull =<< modulesFromPaths
        (pathFromModuleName <$> NonEmptySet.toUnfoldable needed)
    Right modules ->
      pure $ Lazy.force modules

readCoreFnModule :: String -> Aff (Either (Tuple FilePath String) (Tuple (Module Ann) LastTimestamp))
readCoreFnModule filePath = do
  contents <- FS.readTextFile UTF8 filePath
  time <- modifiedTimeMs <$> FS.stat filePath
  case lmap Json.printJsonDecodeError <<< decodeModule =<< Json.jsonParser contents of
    Left err -> do
      pure $ Left $ Tuple filePath err
    Right mod ->
      pure $ Right $ Tuple mod (Just (Max time))

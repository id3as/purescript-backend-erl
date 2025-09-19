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
import Control.Parallel (parTraverse)
import Data.Argonaut (Json, JsonDecodeError)
import Data.Argonaut as Json
import Data.Argonaut.Decode.Decoders (decodeArray, decodeJObject, decodeString, getField)
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.Compactable (separate)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Lazy (Lazy, defer, force)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Ord.Max (Max)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.Regex as Re
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
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
import Node.FS.Stats as Stats
import Node.FS.Stream (createReadStream, createWriteStream)
import Node.Glob.Basic (expandGlobs)
import Node.Library.Execa (ExecaResult, execa)
import Node.Path (FilePath)
import Node.Process as Process
import Node.Stream as Stream
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Erl.Convert.Common (erlModuleNamePs)
import PureScript.Backend.Optimizer.CoreFn (Ann(..), Import(..), Module, ModuleName(..), emptySpan, isPrimModule)
import PureScript.Backend.Optimizer.CoreFn.Json (decodeModule)
import PureScript.Backend.Optimizer.CoreFn.Sort (sortModules)
import Unsafe.Coerce (unsafeCoerce)

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
      FS.writeTextFile UTF8 scriptFile $ String.joinWith "\n"
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
  :: FilePath
  -> NonEmptyArray String
  -> Aff (Either (NonEmptyArray (Tuple FilePath String)) (List ModulePeek))
coreFnModulesFromOutput path globs = runExceptT do
  paths <- Set.toUnfoldable <$> liftAff
    (expandGlobs path ((_ <> "/corefn.json") <$> NonEmptyArray.toArray globs))
  case NonEmptyArray.toArray globs of
    [ "*" ] ->
      adaptSorter sortModules <$> modulesFromPaths paths
    _ ->
      unsafeCrashWith "TODO: reimplement --filter"
      -- go <<< foldl resumePull emptyPull =<< modulesFromPaths paths
  where
  adaptSorter = unsafeCoerce :: forall i. (i (Module Ann) -> List (Module Ann)) -> i ModulePeek -> List ModulePeek

  modulesFromPaths :: _ -> ExceptT (NonEmptyArray (Tuple FilePath String)) Aff _
  modulesFromPaths paths = ExceptT do
    { left, right } <- separate <$> parTraverse readCoreFnModule paths
    case NonEmptyArray.fromArray left of
      Nothing -> pure $ Right right
      Just errors -> pure $ Left errors

  -- pathFromModuleName (ModuleName mn) =
  --   path <> "/" <> mn <> "/corefn.json"

  -- go pull = case pullResult pull of
  --   Left needed ->
  --     go <<< foldl resumePull pull =<< modulesFromPaths
  --       (pathFromModuleName <$> NonEmptySet.toUnfoldable needed)
  --   Right modules ->
  --     pure $ Lazy.force modules

readCoreFnModule :: FilePath -> Aff (Either (Tuple FilePath String) ModulePeek)
readCoreFnModule filePath = do
  contents <- FS.readTextFile UTF8 filePath
  -- time <- modifiedTimeMs <$> FS.stat filePath
  case lmap Json.printJsonDecodeError $ decodeModulePeek contents of
    Left err -> do
      pure $ Left $ Tuple filePath err
    Right mod -> do
      pure $ Right mod

type ModulePeek =
  { name :: ModuleName
  , path :: FilePath
  , imports :: Array (Import Ann)
  , importNames :: Set ModuleName
  , corefn :: String
  , full :: Lazy (Module Ann)
  }

decodeModulePeek :: String -> JsonDecode ModulePeek
decodeModulePeek s = do
  Tuple hit j <- tryFastScan s
  obj <- decodeJObject j
  name <- getField decodeModuleName obj "moduleName"
  path <- getField decodeString obj "modulePath"
  imports <- getField (decodeArray decodeImport) obj "imports"
  let
    importNames = Set.fromFoldable (imports <#> \(Import _ dep) -> dep)
    full = defer \_ -> case decodeModule =<< if hit then Json.parseJson s else pure j of
      Right mod -> mod
      Left err -> unsafeCrashWith $ Json.printJsonDecodeError err
  pure { name, path, imports, importNames, corefn: s, full }

type JsonDecode = Either JsonDecodeError

decodeModuleName :: Json -> JsonDecode ModuleName
decodeModuleName = map (ModuleName <<< String.joinWith ".") <<< decodeArray decodeString

decodeImport :: Json -> JsonDecode (Import Ann)
decodeImport json = do
  obj <- decodeJObject json
  mod <- getField decodeModuleName obj "moduleName"
  pure $ Import (Ann { span: emptySpan, meta: Nothing }) mod


-- Modules can be:
-- - In need of rebuild: hashes don't match, or downstream of a rebuild
-- - An immediate or transitive dependency of a rebuild: load from cache
-- - Inert, not directly related to rebuildable modules
decideModules ::
  { scanned :: List ModulePeek
  , unchanged :: Set ModuleName
  } ->
  { needsRebuild :: Set ModuleName
  , toBeBuilt :: List (Module Ann)
  , pleaseLoadCache :: Set ModuleName
  , alreadyBuilt :: Set ModuleName -- less important
  }
decideModules { scanned, unchanged } = { needsRebuild, toBeBuilt, pleaseLoadCache, alreadyBuilt }
  where
  depMap = Map.fromFoldable $ scanned <#> \{ name, importNames } ->
    Tuple name $ Set.filter (not isPrimModule) $ importNames
  alreadyBuilt = trimming unchanged
  needsRebuild = Set.difference (Map.keys depMap) alreadyBuilt
  toBeBuilt = map (force <<< _.full) $ scanned #
    List.filter \{ name } -> Set.member name needsRebuild
  pleaseLoadCache = expanding Set.empty needsRebuild `Set.difference` needsRebuild

  -- Make sure all transitive deps are unchanged too
  trim moduleSet = moduleSet # Set.filter \name ->
    case Map.lookup name depMap of
      Nothing -> false
      Just deps -> Set.subset deps moduleSet
  trimming moduleSet =
    case trim moduleSet of
      reduced | reduced == moduleSet -> moduleSet
      reduced -> trimming reduced

  -- Get all transitive dependencies
  expanding moduleSet waveFront
    | Set.isEmpty waveFront = moduleSet
    | both <- moduleSet <> waveFront
    , next <- depsOf waveFront =
      expanding both $ Set.difference next both
  depsOf = foldMap (fold <<< flip Map.lookup depMap)

tryFastScan :: String -> Either JsonDecodeError (Tuple Boolean Json)
tryFastScan s = case Re.match fastScanRegex s of
  Just matched
    | Just match <- NEA.head matched
    , Right r <- Json.parseJson ("{"<>match<>"}") -> Right (Tuple true r)
  _ -> Tuple false <$> Json.parseJson s

fastScanRegex :: Re.Regex
fastScanRegex = unsafeRegex fastScanRegexSource mempty

fastScanRegexSource :: String
fastScanRegexSource =
  String.joinWith ","
    [ key "imports" $ array importFragment
    , key "moduleName" $ array str
    , key "modulePath" str
    , key "reExports" $ map $ array str
    , sourceSpan
    ] <> "(?=\\}\\s*$)" -- make sure it is at the end of the object and file
  where
  gr re = "(?:" <> re <> ")"
  star re = gr re <> "*"
  str = "\"(?:[^\\\"]+|\\\\.)\""
  int = "\\d+"
  key k v = show k <> ":" <> v
  array v = "\\[" <> star (v <> ",?") <> "\\]"
  map v = "\\{" <> star (str <> ":" <> v <> ",?") <> "\\}"
  rec kvs = "\\{" <> String.joinWith "," kvs <> "\\}"

  sourceSpan = key "sourceSpan" $ map $ array int
  importFragment = rec
    [ key "annotation" $ rec
      [ key "meta" "null"
      , sourceSpan
      ]
    , key "moduleName" $ array str
    ]


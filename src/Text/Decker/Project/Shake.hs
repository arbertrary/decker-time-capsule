{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Decker.Project.Shake
  ( runDecker,
    calcSource,
    calcSource',
    currentlyServedPages,
    relativeSupportDir,
    putCurrentDocument,
    watchChangesAndRepeat,
    withShakeLock,
    -- runHttpServerIO,
  )
where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM (TChan, newTChan, readTChan, tryReadTChan, writeTChan)
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Catch hiding (try)
import Data.Aeson hiding (Error)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char
import Data.Dynamic
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.Extra as List
import Data.Maybe
import Development.Shake hiding (doesDirectoryExist, putError)
import GHC.IO.Exception (ExitCode (ExitFailure))
import NeatInterpolation
import Relude hiding (state)
import qualified System.Console.GetOpt as GetOpt
import System.Directory as Dir
import System.Environment
import qualified System.FSNotify as Notify
import System.FilePath.Posix
import System.Info
import System.Process hiding (runCommand)
import Text.Decker.Internal.Common
import Text.Decker.Internal.External
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Project.ActionContext
import Text.Decker.Project.Project
import Text.Decker.Resource.Resource
import Text.Decker.Server.Server
import Text.Pandoc hiding (Verbosity)

runDecker :: Rules () -> IO ()
runDecker theRules = do
  args <- getArgs
  let (results, targets, errors) = GetOpt.getOpt GetOpt.Permute deckerFlags args
  let flags = rights results
  let rules =
        if null targets
          then theRules
          else want targets >> withoutActions theRules
  meta <- readMetaDataFile globalMetaFileName
  context <- initContext flags meta
  if not (null errors)
    then do
      mapM_ putStrLn errors
      exitWith (ExitFailure 1)
    else do
      let commands = ["clean", "example", "serve", "pdf"]
      case targets of
        [command] | command `elem` commands -> do
          context <- initContext flags meta
          runCommand context command rules
        _ -> runTargets context targets rules

runTargets :: ActionContext -> [FilePath] -> Rules () -> IO ()
runTargets context targets rules = do
  let flags = context ^. extra
  extractMetaIntoFile (context ^. extra)
  channel <- atomically newTChan

  when (OpenFlag `elem` flags) $ do
    let PortFlag port = fromMaybe (PortFlag 8888) $ find aPort flags
    openBrowser $ "http://localhost:" <> show port <> "/index.html"
  -- Always run at least once
  runShake context rules
  if
      | ServerFlag `elem` flags -> do
        forkServer channel context
        Notify.withManager $ \manager -> do
          startWatcher manager channel context
          runShakeForever channel context rules
      | WatchFlag `elem` flags -> do
        Notify.withManager $ \manager -> do
          startWatcher manager channel context
          runShakeForever channel context rules
      | otherwise -> return ()

data DoOrDie = RunShake | ServerExit String deriving (Show)

-- |  Removes all pending messages from the channel and discards them.
flushTChan' :: TChan a -> STM ()
flushTChan' channel = do
  result <- tryReadTChan channel
  case result of
    Just _ -> flushTChan' channel
    Nothing -> return ()

-- |  Removes all pending messages from the channel and discards them.
flushTChan :: TChan a -> IO ()
flushTChan channel = atomically $ flushTChan' channel

runShake :: ActionContext -> Rules () -> IO ()
runShake context rules = do
  options <- deckerShakeOptions context
  shakeArgsWith options deckerFlags (\_ _ -> return $ Just rules)

runShakeForever :: TChan DoOrDie -> ActionContext -> Rules () -> IO b
runShakeForever channel context rules = do
  forever $ do
    dod <- atomically $ readTChan channel
    case dod of
      RunShake -> do
        catchAll
          (runShake context rules)
          (\(SomeException _) -> return ())
        flushTChan channel
        reloadClients (context ^. server)
      ServerExit port -> do
        putStrLn $ "# Server: " <> show port
        exitWith (ExitFailure 7)

startWatcher :: Notify.WatchManager -> TChan DoOrDie -> ActionContext -> IO ()
startWatcher manager channel context = do
  inDir <- makeAbsolute projectDir
  options <- deckerShakeOptions context
  exclude <- mapM canonicalizePath $ excludeDirs (context ^. globalMeta)
  void $
    Notify.watchTree manager inDir (filter exclude) $ \event ->
      atomically $ writeTChan channel RunShake
  where
    filter exclude event = not $ any (`isPrefixOf` Notify.eventPath event) exclude

startServer :: ActionContext -> IO ()
startServer = runHttpServer

forkServer :: TChan DoOrDie -> ActionContext -> IO ThreadId
forkServer channel context = do
  forkIO $ do
    catchAll
      (startServer context)
      ( \(SomeException e) -> do
          atomically $ writeTChan channel (ServerExit $ show e)
      )

runCommand :: (Eq a, IsString a) => ActionContext -> a -> Rules () -> IO b
runCommand context command rules = do
  case command of
    "clean" -> runClean True
    "example" -> writeExampleProject
    "serve" -> startServer context
    "pdf" -> do
      putStrLn (toString pdfMsg)
      channel <- atomically newTChan
      id <- forkServer channel context
      let rules' = want ["pdf"] >> withoutActions rules
      runShake context rules'
      killThread id
    _ -> error "Unknown command. Should not happen."
  exitSuccess

deckerFlags :: [GetOpt.OptDescr (Either String Flags)]
deckerFlags =
  [ GetOpt.Option
      ['m']
      ["meta"]
      (GetOpt.ReqArg parseMetaValueArg "META")
      "Set a meta value like this: 'name=value'. Overrides values from decker.yaml.",
    GetOpt.Option
      ['w']
      ["watch"]
      (GetOpt.NoArg $ Right WatchFlag)
      "Watch changes to source files and rebuild current target if necessary.",
    GetOpt.Option
      ['S']
      ["server"]
      (GetOpt.NoArg $ Right ServerFlag)
      "Serve the public dir via HTTP (implies --watch).",
    GetOpt.Option
      ['e']
      ["stderr"]
      (GetOpt.NoArg $ Right ErrorFlag)
      "Output errors to stderr.",
    GetOpt.Option
      ['t']
      ["single-thread"]
      (GetOpt.NoArg $ Right ThreadFlag)
      "Run single threaded.",
    GetOpt.Option
      ['o']
      ["open"]
      (GetOpt.NoArg $ Right OpenFlag)
      "Open the webbrowser.",
    GetOpt.Option
      ['p']
      ["port"]
      (GetOpt.ReqArg parsePortArg "PORT")
      "The HTTP server listens on the given port.",
    GetOpt.Option
      ['b']
      ["bind"]
      (GetOpt.ReqArg (Right . BindFlag) "BIND")
      "Bind the HTTP server to given address."
  ]

parsePortArg :: String -> Either String Flags
parsePortArg arg =
  case readMaybe arg of
    Just port | port > 2 ^ 10 && port <= 2 ^ 16 -> Right $ PortFlag port
    _ -> Left "Cannot parse argument. Must be an integer from (2^10, 2^16]."

parseMetaValueArg :: String -> Either String Flags
parseMetaValueArg arg =
  case List.splitOn "=" arg of
    [meta, value]
      | isMetaName meta -> Right $ MetaValueFlag meta value
    _ -> Left "Cannot parse argument. Must be 'name=value'."

isMetaName :: String -> Bool
isMetaName str = all check $ List.splitOn "." str
  where
    check s = length s > 1 && isAlpha (List.head s) && all (\c -> isAlphaNum c || isSymbol c || isPunctuation c) (List.tail s)

-- | Saves the meta flags to a well known file. Will be later read and cached by
-- shake.
extractMetaIntoFile :: [Flags] -> IO ()
extractMetaIntoFile flags = do
  let metaFlags = HashMap.fromList $ map (\(MetaValueFlag k v) -> (k, v)) $ filter aMetaValue flags
  let json = decodeUtf8 $ encode metaFlags
  writeFileChanged metaArgsFile json

aMetaValue (MetaValueFlag _ _) = True
aMetaValue _ = False

initContext :: [Flags] -> Meta -> IO ActionContext
initContext extra meta = do
  createDirectoryIfMissing True transientDir
  devRun <- isDevelopmentRun
  external <- checkExternalPrograms
  server <- newMVar ([], fromList [])
  watch <- newIORef False
  public <- newResourceIO "public" 1
  chan <- atomically newTChan
  return $ ActionContext extra devRun external server watch chan public meta

watchChangesAndRepeat :: Action ()
watchChangesAndRepeat = do
  context <- actionContext
  liftIO $ watchChangesAndRepeatIO context

watchChangesAndRepeatIO :: ActionContext -> IO ()
watchChangesAndRepeatIO context = do
  let ref = context ^. watch
  liftIO $ writeIORef ref True

-- | Outputs errors to stderr if the `-e` flag is provided. Otherwise everything
-- goes to stdout.
outputMessage :: Bool -> Verbosity -> String -> IO ()
outputMessage mode verbosity text = do
  let stream = if mode && verbosity == Error then stderr else stdout
  BS.hPutStrLn stream $ UTF8.fromString text

deckerShakeOptions :: ActionContext -> IO ShakeOptions
deckerShakeOptions ctx = do
  let single = ThreadFlag `elem` (ctx ^. extra)
  let toStderr = ErrorFlag `elem` (ctx ^. extra)
  return $
    shakeOptions
      { shakeFiles = transientDir,
        shakeExtra = HashMap.insert actionContextKey (toDyn ctx) HashMap.empty,
        shakeThreads = if single then 1 else 0,
        shakeColor = not toStderr,
        shakeStaunch = toStderr,
        shakeOutput = outputMessage toStderr,
        shakeChange = ChangeModtimeAndDigest
        -- shakeLint = Just LintBasic,
        -- shakeReport = [".decker/shake-report.html"],
        -- shakeChange = ChangeModtime,
      }

relativeSupportDir :: FilePath -> FilePath
relativeSupportDir from = makeRelativeTo from supportDir

withShakeLock :: Action a -> Action a
withShakeLock perform = do
  context <- actionContext
  let resource = context ^. publicResource
  withResource resource 1 perform

-- | Returns a list of all pages currently served, if any.
currentlyServedPages :: Action [FilePath]
currentlyServedPages = do
  context <- actionContext
  (_, pages) <- liftIO $ readMVar (context ^. server)
  return $ toList pages

openBrowser :: String -> IO ()
openBrowser url =
  if
      | any (`List.isInfixOf` os) ["linux", "bsd"] ->
        liftIO $ callProcess "xdg-open" [url]
      | "darwin" `List.isInfixOf` os -> liftIO $ callProcess "open" [url]
      | otherwise ->
        putStrLn $ "Unable to open browser on this platform for url: " ++ url

calcSource :: String -> String -> FilePath -> Action FilePath
calcSource targetSuffix srcSuffix target = do
  let src =
        (replaceSuffix targetSuffix srcSuffix . makeRelative publicDir) target
  need [src]
  return src

-- |  calcSource without the call to need and without the suffix replacement
calcSource' :: FilePath -> Action FilePath
calcSource' target = do
  return $ makeRelative publicDir target

putCurrentDocument :: FilePath -> Action ()
putCurrentDocument out = putInfo $ "# pandoc (for " ++ out ++ ")"

-- | Functionality for "decker clean" command Removes public and .decker
-- directory. Located outside of Shake due to unlinking differences and
-- parallel processes on Windows which prevented files (.shake.lock) from being
-- deleted on Windows.
runClean :: Bool -> IO ()
runClean totally = do
  warnVersion
  putStrLn $ "# Removing " ++ publicDir
  tryRemoveDirectory publicDir
  when totally $
    do
      putStrLn $ "# Removing " ++ transientDir
      tryRemoveDirectory transientDir

pdfMsg =
  [text|
    # 
    # To use 'decker pdf' Google Chrome has to be installed.
    # 
    # Windows: Currently 'decker pdf' does not work on Windows.
    #   Please add 'print: true' or 'menu: true' to your slide deck and use
    #   the print button on the title slide.
    #
    # MacOS: Follow the Google Chrome installer instructions.
    #   'Google Chrome.app' has to be located in either of these locations
    #
    #   - '/Applications/Google Chrome.app' 
    #   - '/Users/<username>/Applications/Google Chrome.app'
    #
    # Linux: 'chrome' has to be on $$PATH.
    # 
  |]

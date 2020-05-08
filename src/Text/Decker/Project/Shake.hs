{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Text.Decker.Project.Shake
  ( runDecker
  , currentlyServedPages
  , isDevRun
  , openBrowser
  , publicResource
  , publicResourceA
  , putCurrentDocument
  , readStaticMetaData
  , runHttpServer
  , startHttpServer
  , stopHttpServer
  , watchChangesAndRepeat
  -- , withShakeLock
  , writeSupportFilesToPublic
  , getAdditionalMeta
  ) where

import Text.Decker.Internal.Common
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Internal.URI
import Text.Decker.Project.Project
import Text.Decker.Resource.Template
import Text.Decker.Server.Server

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Data.Char
import Data.Dynamic
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.List
import Data.List.Extra
import Data.Maybe
import qualified Data.Set as Set
import Data.Typeable
import Development.Shake hiding (doesDirectoryExist, putError)
import System.Console.GetOpt
import System.Directory as Dir
import qualified System.FSNotify as Notify
import System.FilePath
import System.Info
import System.Process
import Text.Pandoc hiding (lookupMeta)

instance Show (IORef a) where
  show _ = "IORef"

data MutableActionState = MutableActionState
  { _devRun :: Bool
  , _server :: IORef (Maybe Server)
  , _watch :: IORef Bool
  , _publicResource :: Development.Shake.Resource
  } deriving (Show)

makeLenses ''MutableActionState

data ActionContext = ActionContext
  { _state :: MutableActionState
  } deriving (Typeable, Show)

makeLenses ''ActionContext

initMutableActionState = do
  devRun <- isDevelopmentRun
  server <- newIORef Nothing
  watch <- newIORef False
  public <- newResourceIO "public" 1
  return $ MutableActionState devRun server watch public

runDecker :: Rules () -> IO ()
runDecker rules = do
  state <- initMutableActionState
  catchAll (repeatIfTrue $ runShakeOnce state rules) (putError "Terminated: ")
  cleanup state

data Flags
  = MetaValueFlag String
                  String
  | FormatFlag
  deriving (Eq, Show)

deckerFlags :: [OptDescr (Either String Flags)]
deckerFlags =
  [ Option
      ['m']
      ["meta"]
      (ReqArg parseMetaValueArg "META")
      "Set a meta value like this: 'name=value'. Overrides values from decker.yaml."
  , Option
      ['f']
      ["format"]
      (NoArg $ Right FormatFlag)
      "Format Markdown on stdin to stdout"
  ]

parseMetaValueArg :: String -> Either String Flags
parseMetaValueArg arg =
  case splitOn "=" arg of
    [meta, value]
      | isMetaName meta -> Right $ MetaValueFlag meta value
    _ -> Left "Cannot parse argument. Must be 'name=value'."

isMetaName :: String -> Bool
isMetaName str = all check $ splitOn "." str
  where
    check s = length s > 1 && isAlpha (head s) && all isAlphaNum (tail s)

-- TODO: Handle the meta flag
handleArguments :: Rules () -> [Flags] -> [String] -> IO (Maybe (Rules ()))
handleArguments rules flags targets =
  return $ Just $
  if null targets
    then rules
    else want targets >> withoutActions rules

runShakeOnce :: MutableActionState -> Rules () -> IO Bool
runShakeOnce state rules = do
  dirs <- projectDirectories
  createDirectoryIfMissing True (dirs ^. transient)
  context <- initContext state
  options <- deckerShakeOptions dirs context
  catchAll
    (shakeArgsWith options deckerFlags (handleArguments rules))
    (putError "Error: ")
  server <- readIORef (state ^. server)
  forM_ server reloadClients
  keepWatching <- readIORef (state ^. watch)
  when keepWatching $ do
    let projectDir = dirs ^. project
    meta <- readMetaDataFile (projectDir </> "decker.yaml")
    let exclude = map (projectDir </>) $ excludeDirs meta
    waitForChange projectDir exclude
  return keepWatching

initContext :: MutableActionState -> IO ActionContext
initContext state = do
  return $ ActionContext state

cleanup state = do
  srvr <- readIORef $ state ^. server
  forM_ srvr stopHttpServer

watchChangesAndRepeat :: Action ()
watchChangesAndRepeat = do
  ref <- _watch . _state <$> actionContext
  liftIO $ writeIORef ref True

putError :: String -> SomeException -> IO ()
putError prefix (SomeException e) = putStrLn $ prefix ++ show e

deckerShakeOptions :: ProjectDirs -> ActionContext -> IO ShakeOptions
deckerShakeOptions dirs ctx = do
  cores <- getNumCapabilities
  return $
    shakeOptions
      { shakeFiles = dirs ^. transient
      , shakeExtra = HashMap.insert actionContextKey (toDyn ctx) HashMap.empty
      , shakeThreads = cores
      -- , shakeChange = ChangeModtimeAndDigest
      , shakeAbbreviations = [(dirs ^. project ++ "/", "/")]
      }

actionContextKey :: TypeRep
actionContextKey = typeOf (undefined :: ActionContext)

actionContext :: Action ActionContext
actionContext =
  fromMaybe (error "Error getting action context") <$> getShakeExtra

waitForChange :: FilePath -> [FilePath] -> IO ()
waitForChange inDir exclude =
  Notify.withManager
    (\manager -> do
       done <- newEmptyMVar
       Notify.watchTree manager inDir filter (\e -> putMVar done ())
       takeMVar done)
  where
    filter event = not $ any (`isPrefixOf` Notify.eventPath event) exclude

isDevRun :: Action Bool
isDevRun = do
  context <- actionContext
  return (context ^. state . devRun)

writeSupportFilesToPublic :: Meta -> Action ()
writeSupportFilesToPublic meta = do
  templateSource <-
    liftIO $ calcTemplateSource (lookupMeta "template-source" meta)
  let support = supportDir meta
  correct <- correctSupportInstalled support templateSource
  if correct
    then putNormal "# support files up to date"
    else do
      putNormal $ "# copy support files from: " <> show templateSource
      removeSupport support
      extractSupport support templateSource

extractSupport :: FilePath -> TemplateSource -> Action ()
extractSupport support templateSource = do
  context <- actionContext
  liftIO $ do
    copySupportFiles templateSource Copy support
    writeFile (support </> ".origin") (show templateSource)

correctSupportInstalled :: FilePath -> TemplateSource -> Action Bool
correctSupportInstalled support templateSource = do
  liftIO $ handleAll (\_ -> return False) $ do
    installed <- read <$> readFile (support </> ".origin")
    return (installed == templateSource)

removeSupport :: FilePath -> Action ()
removeSupport support = do
  liftIO $ handleAll (\_ -> return ()) $ removeDirectoryRecursive support

publicResourceA = _publicResource . _state <$> actionContext

{-
 -withShakeLock :: Action a -> Action a
 -withShakeLock perform = do
 -  r <- _publicResource . _state <$> actionContext
 -  withResource r 1 perform
 -}
-- | Runs the built-in server on the given directory, if it is not already
-- running.
runHttpServer :: Int -> ProjectDirs -> Maybe String -> Action ()
runHttpServer port dirs url = do
  ref <- _server . _state <$> actionContext
  server <- liftIO $ readIORef ref
  case server of
    Just _ -> return ()
    Nothing -> do
      httpServer <- liftIO $ startHttpServer dirs port
      liftIO $ writeIORef ref $ Just httpServer
      forM_ url openBrowser

-- | Returns a list of all pages currently served, if any.
currentlyServedPages :: Action [FilePath]
currentlyServedPages = do
  ref <- _server . _state <$> actionContext
  server <- liftIO $ readIORef ref
  case server of
    Just (_, state) -> do
      (_, pages) <- liftIO $ readMVar state
      return $ Set.toList pages
    Nothing -> return []

openBrowser :: String -> Action ()
openBrowser url =
  if | any (`isInfixOf` os) ["linux", "bsd"] ->
       liftIO $ callProcess "xdg-open" [url]
     | "darwin" `isInfixOf` os -> liftIO $ callProcess "open" [url]
     | otherwise ->
       putNormal $ "Unable to open browser on this platform for url: " ++ url

{-
 -calcSource :: String -> String -> FilePath -> Action FilePath
 -calcSource targetSuffix srcSuffix target
 - = do
 -  dirs <- projectDirsA
 -  let src =
 -        (replaceSuffix targetSuffix srcSuffix . combine (dirs ^. project) .
 -         makeRelative (dirs ^. public))
 -          target
 -  need [src]
 -  return src
 -
 --- |  calcSource without the call to need and without the suffix replacement
 -calcSource' :: FilePath -> Action FilePath
 -calcSource' target
 - = do
 -  dirs <- projectDirsA
 -  return $ dirs ^. project </> makeRelative (dirs ^. public) target
 -}
putCurrentDocument :: FilePath -> Action ()
putCurrentDocument out = putNormal $ "# pandoc (for " ++ out ++ ")"

-- Check for additional meta files specified in the Meta option `meta-data`.
-- Assumes that file are specified with absolute paths.
getAdditionalMeta :: FilePath -> Meta -> Action Meta
getAdditionalMeta project meta = do
  let moreFiles = lookupMetaOrElse [] "meta-data" meta
  moreMeta <- traverse (readMetaData project) moreFiles
  return $ foldr mergePandocMeta' meta (reverse moreMeta)

-- | Reads a meta data file. All values that are paths to local project files are
-- made absolute. Files referenced in `meta-data` are recursively loaded and merged.
readMetaData :: FilePath -> FilePath -> Action Meta
readMetaData project file = do
  need [file]
  putVerbose $ "# reading meta data from: " <> file
  let base = takeDirectory file
  meta <-
    liftIO $
    (readMetaDataFile file >>= mapMeta (makeAbsolutePathIfLocal project base))
  getAdditionalMeta project meta

-- | Reads static meta data from the `decker.yaml` file in the project root.
-- Also reads meta data from files listed in `meta-data`
readStaticMetaData :: ProjectDirs -> FilePath -> Action Meta
readStaticMetaData dirs file = do
  meta <-
    setMetaValue "decker.directories" dirs <$>
    readMetaData (dirs ^. project) file
  templateSource <-
    liftIO $ calcTemplateSource (lookupMeta "template-source" meta)
  defaultMeta <- readTemplateMeta templateSource
  return $ mergePandocMeta' meta defaultMeta

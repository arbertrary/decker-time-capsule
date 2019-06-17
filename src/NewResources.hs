{-- Author: Armin Bernstetter <bernstetter@informatik.uni-wuerzburg.de --}
-- | This module is an interface that provides transparent access to the resources
-- Depending on specification in "decker-meta.yaml" the source of the resource folder is chosen
-- Everything that is copying or linking Resource folders needs to be moved here
-- 
module NewResources
  -- * Provisioning
  ( handleResources
  , testResources
  , extractNResources
  , getResourceType
  , downloadResources
  ) where

import Common

-- import Exception
import Meta
import Project

import Shake

-- import System.Decker.OS
import Codec.Archive.Zip
import Control.Exception
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Extra
import Data.ByteString.Lazy as BS (writeFile)
import Data.Map.Strict (size)

-- import qualified Data.Text as T
-- import Development.Shake
-- import System.Environment
import Data.Yaml as Yaml
import Network.HTTP.Conduit (simpleHttp)
import Network.URI
import qualified System.Directory as Dir
import System.FilePath

-- import Text.URI (mkURI)
-- import Text.URI.Lens
{- NOTE:
resources is already in meta data (read from decker-meta.yaml)
see metaA in Shake.hs
this metadata is currently only read in Decker.hs and then propagated forward
This should stay. Here in NewResources only the ResourceType Datatype should be used.
Paths come from Project.hs

- call one "main" function from Decker.hs
- must account for "example", "support" and "template"
- must account for each ResourceType
- must account for each Provisioning variant (copy, symlink etc)

- There is a difference between the provisioning that is done in the "provision..." functions
and the copying/linking that is done when "decker support" or "decker example" happens.
provisionResources happens during the processing of markdown files

- the only directory path that has to be changed is 
(directories ^. appData)
e.g.  /home/user/.local/share/decker-0.7.3-84-resource-handling-d804821
Folder contains example, support, template folders. from there on the rest can stay the same
- COULD be changed using directories & appData .~ path
- For ResourceTypes without caching the public and support dirs could be changed accordingly
- what about the template dir? where is it called from?
I don't think it is copied anywhere so if the appData directory is correct it should not matter


Strategy for different ResourceTypes:
- Decker/Default -> same as currently. unpack from executable, unpack to .local/share
- File -> unpack from local zip archive (absolute or relative path?), unpack to .local/share
- Https -> curl/wget zip archive and immediately unpack to .local/share
- Dev -> Use resource folder here in the decker repo
- Project -> Use resource folder in slide project (no copying/symlinking to public?)
- Local -> Use resource folder anywhere
-}
getResourceType :: Yaml.Value -> ResourceType
getResourceType meta =
  case metaValueAsString "resources" meta of
    Just resources ->
      case parseURIReference resources of
        Just (URI "file:" _ path _ _) -> fileOrLocal path
        Just (URI "https:" _ path _ _) -> Https resources
        Just (URI _ _ path _ _) -> fileOrLocal path
        _ -> Decker
      where fileOrLocal path =
              if ".zip" `isExtensionOf` path
                then File path
                else Local path
    Nothing -> Decker

{-
Resources for testing
https://downloads.hci.informatik.uni-wuerzburg.de/decker/resourcetest/resource.zip
http://downloads.hci.informatik.uni-wuerzburg.de/decker/resourcetest/resource.zip
/Users/armin/work/decker_related/resourcetest/resource.zip
/Users/armin/work/decker_related/resourcetest/resource


-}
testResources :: Yaml.Value -> IO ()
testResources meta = do
  let rt = getResourceType meta
  print "## Resource Type:"
  print rt
  case rt of
    File path -> extractNResources path
    _ -> print "no zip"
  p <- testdeckerResourceDir rt
  absp <- Dir.makeAbsolute p
  print "### Absolute:"
  pexists <- Dir.doesDirectoryExist absp
  print absp

handleResources :: IO ProjectDirs
handleResources = do
  directories <- projectDirectories
  meta <- readMetaData $ directories ^. project
  defaultResourceDir <- deckerResourceDir
  let rt = getResourceType meta
  print rt
  -- print directories
  case rt of
    File path -> extractNResources path >> return directories
    Https url -> do
      putStrLn $ "Downloading resources from " ++ url
      downloadResources url
      return directories
    Local path -> do
      print path
      let t = (directories & appData .~ path)
      print t
      return t
    _ -> extractNResources defaultResourceDir >> return directories

-- | Download from url, write to System temp dir and extract from there
downloadResources :: String -> IO ()
downloadResources url = do
  bs <- simpleHttp url
  cache <- Dir.getTemporaryDirectory
  let resfile = cache </> "zresource.zip"
  BS.writeFile resfile bs
  extractNResources resfile
  Dir.removeFile resfile

-- handleResources :: Action Yaml.Value -> Action ProjectDirs
-- handleResources meta = do
--   m <- meta
--   directories <- liftIO projectDirectories
--   defaultResourceDir <- liftIO deckerResourceDir
--   let rt = getResourceType m
--   case rt of
--     File path -> liftIO (extractNResources path >> return directories)
--     Https url ->
--       liftIO (extractNResources defaultResourceDir >> return directories)
--     Local path -> return (directories & appData .~ path)
--     _ -> liftIO (extractNResources defaultResourceDir >> return directories)
-- import Text.Regex.TDFA
-- | Extract resources from the executable into the XDG data directory.
-- TODO: Simply add parameter instead of calling getExecutablePath inside
extractNResources :: FilePath -> IO ()
extractNResources zipFile = do
  dataDir <- deckerResourceDir
  exists <- Dir.doesDirectoryExist dataDir
  unless exists $ do
    numFiles <- withArchive zipFile getEntries
    unless ((size numFiles) > 0) $
      throw $ ResourceException "No resource zip found in decker executable."
    Dir.createDirectoryIfMissing True dataDir
    withArchive zipFile (unpackInto dataDir)
    putStrLn $ "# resources extracted to " ++ dataDir

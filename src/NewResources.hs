{-- Author: Armin Bernstetter <bernstetter@informatik.uni-wuerzburg.de --}
-- | This module is an interface that provides transparent access to the resources
-- Depending on specification in "decker-meta.yaml" the source of the resource folder is chosen
-- Everything that is copying or linking Resource folders needs to be moved here
-- 
module NewResources
  -- * Provisioning
  ( handleResources
  , extractNResources
  , getResourceType
  , getResourceMeta
  , downloadResources
  , getInplaceMeta
  ) where

import Common
import Meta
import Project
import Shake

import Codec.Archive.Zip
import Control.Exception
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Extra
import Data.ByteString.Lazy as BS (writeFile)
import Data.List.Utils (replace)
import Data.Map.Strict (size)
import Data.Yaml as Yaml
import Network.HTTP.Conduit (simpleHttp)
import Network.URI
import qualified System.Directory as Dir
import System.Environment
import System.FilePath

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
-- | Find "resources" field in Yaml Metadata
getResourceMeta :: Yaml.Value -> ResourceType
getResourceMeta meta =
  case metaValueAsString "resources" meta of
    Just resources -> getResourceType resources
    Nothing -> Decker

getInplaceMeta :: Yaml.Value -> Bool
getInplaceMeta meta =
  case metaValueAsString "inplace" meta of
    Just "True" -> True
    Just "true" -> True
    Just "1.0" -> True
    Nothing -> False
    _ -> False

-- | Extra Function to parse the resources URI
getResourceType :: String -> ResourceType
getResourceType resources =
  case parseURIReference resources of
    Just (URI "file:" _ path _ _) -> fileOrLocal path
    Just (URI "https:" _ path _ _) -> Https resources
    Just (URI _ _ path _ _) -> fileOrLocal path
    _ -> Decker
  where
    fileOrLocal path =
      if not (null path)
        then if ".zip" `isExtensionOf` path
               then File path
               else Local path
        else Decker

{-
Resources for testing
https://downloads.hci.informatik.uni-wuerzburg.de/decker/resourcetest/resource.zip
http://downloads.hci.informatik.uni-wuerzburg.de/decker/resourcetest/resource.zip
/Users/armin/work/decker_related/resourcetest/resource.zip
/Users/armin/work/decker_related/resourcetest/resource


-}
-- | Main function that calculates the Resource locations depending on the presence of the "resources" metadata field
handleResources :: IO ProjectDirs
handleResources = do
  directories <- projectDirectories
  meta <- readMetaData $ directories ^. project
  defaultResourceDir <- deckerResourceDir
  deckerExecutable <- getExecutablePath
  let rt = getResourceMeta meta
  print rt
  case rt of
    File path -> extractNResources path >> return directories
    Https url -> do
      downloadResources url
      return directories
    Local path -> do
      let dirs = directories & appData .~ path
      return dirs
    _ -> extractNResources deckerExecutable >> return directories
  if getInplaceMeta meta
    then return $ makeInplace directories
    else return directories

-- | replace the "public" in in several in the three ProjectDirs "public", "cache" and "support"
-- Updates the paths using Lens setters and getters
makeInplace :: ProjectDirs -> ProjectDirs
makeInplace directories = b & public .~ (repl (b ^. public))
  where
    a = directories & support .~ (repl (directories ^. support))
    b = a & cache .~ (repl (a ^. cache))
    repl path = joinPath $ removePublic (splitDirectories path)
    removePublic [] = []
    removePublic (y:ys) =
      if "public" == y
        then ys
        else y : removePublic ys

-- | Download from url, write to System temp dir and extract from there
downloadResources :: String -> IO ()
downloadResources url = do
  dataDir <- deckerResourceDir
  exists <- Dir.doesDirectoryExist dataDir
  if exists
    then putStrLn $ dataDir ++ " already exists"
    else do
      putStrLn $ "Downloading resources from " ++ url
      bs <- simpleHttp url
      cache <- Dir.getTemporaryDirectory
      let resfile = cache </> "zresource.zip"
      BS.writeFile resfile bs
      extractNResources resfile
      Dir.removeFile resfile

-- | Extract resources from the executable into the XDG data directory.
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

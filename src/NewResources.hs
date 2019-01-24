{-- Author: Armin Bernstetter <bernstetter@informatik.uni-wuerzburg.de --}
-- | This module is an interface that provides transparent access to the resources
-- Depending on specification in "decker-meta.yaml" the source of the resource folder is chosen
-- Everything that is copying or linking Resource folders needs to be moved here
-- 
module NewResources
  ( writeExampleProject
  , copyDir
  , copyResource
  , linkResource
  , urlToFilePathIfLocal
  -- * Provisioning
  , provisionResource
  , provisionMetaResource
  , provisionTemplateOverrideSupport
  , provisionTemplateOverrideSupportTopLevel
  , handleResources
  ) where

import Common
import Exception
import Meta
import Project
import Shake
import System.Decker.OS

import Codec.Archive.Zip
import Control.Exception
import Control.Lens ((^.))
import Control.Monad.Extra
import Data.Map.Strict (size)
import qualified Data.Text as T
import Data.Yaml as Yaml
import Development.Shake
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
getResourceMeta :: Yaml.Value -> ResourceType
getResourceMeta meta =
  case metaValueAsString "resources" meta of
    Just "project" -> Project $ "." </> "resource"
    Just resources ->
      case parseURI resources of
        Just (URI "file:" _ path _ _) ->
          if ".zip" `isExtensionOf` path
            then File path
            else Local path
        Just (URI "https:" _ path _ _) -> Https resources
        Just (URI "project:" _ path _ _) -> Project $ path </> "resource"
        _ -> Decker
      --   (Just "dev", _) -> Dev
      --   (Just "project", _) -> Project
      --   (_, _) -> Decker
      -- where t = lookupYamlValue "type" resources
      --       src = lookupYamlValue "source" resources
    Nothing -> Decker

handleResources :: Yaml.Value -> IO ()
handleResources meta = do
  let rt = getResourceMeta meta
  print rt
  p <- testdeckerResourceDir rt
  absp <- Dir.makeAbsolute p
  pexists <- Dir.doesDirectoryExist absp
  print absp
  print pexists
  cont <- Dir.getDirectoryContents absp
  print cont

-- | Extract resources from a given .zip archive
extractResources :: FilePath -> IO ()
extractResources archPath
  -- deckerExecutable <- getExecutablePath
 = do
  dataDir <- deckerResourceDir
  exists <- Dir.doesDirectoryExist dataDir
  unless exists $ do
    numFiles <- withArchive archPath getEntries
    unless ((size numFiles) > 0) $
      throw $ ResourceException "No resource zip found in decker executable."
    Dir.createDirectoryIfMissing True dataDir
    withArchive archPath (unpackInto dataDir)
    putStrLn $ "# resources extracted to " ++ dataDir

{-
TODO: From Decker.hs
move functionality from "support" command to here
    checks if support directory exists and checks provisioning type
    symlinks/copies accordingly
-}
{-
TODO: from Resource.hs
writeExampleProject - (done)
writeResourceFiles - (done)
copyDir - (done)
cp - replaced by copyFileIfNewer
getOldResources - (done) 
    (maybe to Project bc it returns Paths?)
    Returns IO [FilePath] containing the paths of all old, cached resource folders
getResourceString (not sure if this fits here. 
    outputs a file as string. e.g. the help-page
    maybe add an entire "Output.hs" module?)
extractResources
    Extract resources from the executable into the XDG data directory
    Currently using cli command unzip
unzip
    calls "unzip"
writeResourceFiles - (done)
-}
-- | Write the example project to the current folder
writeExampleProject :: IO ()
writeExampleProject = writeResourceFiles "example" "."

writeResourceFiles :: FilePath -> FilePath -> IO ()
writeResourceFiles prefix destDir = do
  dataDir <- deckerResourceDir
  let src = dataDir </> prefix
  copyDir src destDir

-- After moving functionality of "decker support" (in Decker.hs) to NewResources module
-- hide this function (this is pure utility)
-- | Copy a directory and its contents recursively
copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
  unlessM (Dir.doesDirectoryExist src) $
    throw (userError "src does not exist or is not a directory")
  dstExists <- Dir.doesDirectoryExist dst
  if dstExists && (last (splitPath src) /= last (splitPath dst))
    then copyDir src (dst </> last (splitPath src))
    else do
      Dir.createDirectoryIfMissing True dst
      contents <- Dir.listDirectory src
      forM_ contents $ \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name
        isDirectory <- Dir.doesDirectoryExist srcPath
        if isDirectory
          then copyDir srcPath dstPath
          else copyFileIfNewer srcPath dstPath

{-
| TODO: From Project.hs
copyFileIfNewer - (done)
fileIsNewer - (done)
copyResource - (done)
linkResource - (done)
what about
absRefResource, relRefResource?
-}
-- | Copies the src to dst if src is newer or dst does not exist. Creates
-- missing directories while doing so.
copyFileIfNewer :: FilePath -> FilePath -> IO ()
copyFileIfNewer src dst =
  whenM (fileIsNewer src dst) $ do
    Dir.createDirectoryIfMissing True (takeDirectory dst)
    Dir.copyFile src dst

fileIsNewer :: FilePath -> FilePath -> IO Bool
fileIsNewer a b = do
  aexists <- Dir.doesFileExist a
  bexists <- Dir.doesFileExist b
  if bexists
    then if aexists
           then do
             at <- Dir.getModificationTime a
             bt <- Dir.getModificationTime b
             return (at > bt)
           else return False
    else return aexists

{-
TODO: These two functions (together with provisionResource and its called functions
from Utilities.hs) should maybe be pulled apart?
Not sure how I feel about a function returning a FilePath in this module
-}
-- | Copies single Resource file and returns Filepath
copyResource :: Project.Resource -> IO FilePath
copyResource resource = do
  copyFileIfNewer (sourceFile resource) (publicFile resource)
  return (publicUrl resource)

-- | Creates SymLink to single resource file and returns Filepath
linkResource :: Project.Resource -> IO FilePath
linkResource resource = do
  whenM
    (Dir.doesFileExist (publicFile resource))
    (Dir.removeFile (publicFile resource))
  Dir.createDirectoryIfMissing True (takeDirectory (publicFile resource))
  Dir.createFileLink (sourceFile resource) (publicFile resource)
  return (publicUrl resource)

{-
| TODO: From Utilities.hs
getSupportDir (move to Project.hs b/c it returns a path)
provisionResources
    step in the pipeline of readAndProcessMarkdown
    type Decker = StateT DeckerState Action
    state transformer 
provisionMetaResource
    what does this do?
    returns an Action FilePath

provisionTemplateOverrideSupport -(done)
provisionTemplateOverrideSupportTopLevel (done)
    what do these two do?
provisionResource (done)
putCurrentDocument
    printing?
urlToFilePathIfLocal (moved to here. not sure about that) (done)
-}
provisionMetaResource ::
     FilePath -> Provisioning -> (String, FilePath) -> Action FilePath
provisionMetaResource base method (key, url)
  | key `elem` runtimeMetaKeys = do
    filePath <- urlToFilePathIfLocal base url
    provisionResource base method filePath
provisionMetaResource base method (key, url)
  | key `elem` templateOverrideMetaKeys = do
    cwd <- liftIO $ Dir.getCurrentDirectory
    filePath <- urlToFilePathIfLocal cwd url
    provisionTemplateOverrideSupportTopLevel cwd method filePath
provisionMetaResource base _ (key, url)
  | key `elem` compiletimeMetaKeys = do
    filePath <- urlToFilePathIfLocal base url
    need [filePath]
    return filePath
provisionMetaResource _ _ (key, url) = return url

provisionTemplateOverrideSupport ::
     FilePath -> Provisioning -> FilePath -> Action ()
provisionTemplateOverrideSupport base method url = do
  let newBase = base </> url
  exists <- liftIO $ Dir.doesDirectoryExist url
  if exists
    then liftIO (Dir.listDirectory url) >>= mapM_ recurseProvision
    else do
      need [url]
      provisionResource base method url
      return ()
  where
    recurseProvision x = provisionTemplateOverrideSupport url method (url </> x)

provisionTemplateOverrideSupportTopLevel ::
     FilePath -> Provisioning -> FilePath -> Action FilePath
provisionTemplateOverrideSupportTopLevel base method url = do
  liftIO (Dir.listDirectory url) >>= filterM dirFilter >>=
    mapM_ recurseProvision
  return $ url
  where
    dirFilter x = liftIO $ Dir.doesDirectoryExist (url </> x)
    recurseProvision x = provisionTemplateOverrideSupport url method (url </> x)

-- | Determines if a URL can be resolved to a local file. Absolute file URLs are
-- resolved against and copied or linked to public from 
--    1. the project root 
--    2. the local filesystem root 
--
-- Relative file URLs are resolved against and copied or linked to public from 
--
--    1. the directory path of the referencing file 
--    2. the project root Copy and link operations target the public directory
--       in the project root and recreate the source directory structure. 
--
-- This function is used to provision resources that are used at presentation
--       time.
--
-- Returns a public URL relative to base
provisionResource :: FilePath -> Provisioning -> FilePath -> Action FilePath
provisionResource base method filePath =
  case parseRelativeReference filePath of
    Nothing ->
      if hasDrive filePath
        then do
          dirs <- projectDirsA
          let resource =
                Resource
                  { sourceFile = filePath
                  , publicFile =
                      (dirs ^. public) </>
                      makeRelativeTo (dirs ^. project) filePath
                  , publicUrl = urlPath $ makeRelativeTo base filePath
                  }
          provision resource
        else return filePath
    Just uri -> do
      dirs <- projectDirsA
      let path = uriPath uri
      fileExists <- doesFileExist path
      if fileExists
        then do
          need [path]
          let resource = resourcePaths dirs base uri
          provision resource
        else throw $ ResourceException $ "resource does not exist: " ++ path
  where
    provision resource = do
      publicResource <- publicResourceA
      withResource publicResource 1 $
        liftIO $
        case method of
          Copy -> copyResource resource
          SymLink -> linkResource resource
          Absolute -> absRefResource resource
          Relative -> relRefResource base resource

urlToFilePathIfLocal :: FilePath -> FilePath -> Action FilePath
urlToFilePathIfLocal base uri =
  case parseRelativeReference uri of
    Nothing -> return uri
    Just relativeUri -> do
      let filePath = uriPath relativeUri
      absBase <- liftIO $ Dir.makeAbsolute base
      absRoot <- projectA
      let absPath =
            if isAbsolute filePath
              then absRoot </> makeRelative "/" filePath
              else absBase </> filePath
      return absPath

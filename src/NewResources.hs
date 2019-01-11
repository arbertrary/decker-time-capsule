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
  ) where

import Common
import Exception
import Project
import Shake

import Control.Exception
import Control.Monad.Extra
import Development.Shake
import Network.URI
import qualified System.Directory as Dir
import System.FilePath

{- NOTE:
Plan:
First move all resource-related functions to this module
then refactor and design new interface

resources is already in meta data (read from decker-meta.yaml)
see metaA in Shake.hs
this metadata is currently only read in Decker.hs and then propagated forward
This should stay. Here in NewResources only the ResourceType Datatype should be used.
Paths come from Project.hs
What does "provision" actually mean in this context?

TODO: From Decker.hs
move functionality from "support" command to here
    (done)s if support directory exists and (done)s provisioning type
    symlinks/copies accordingly

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
    Nothing -> return filePath
    Just uri -> do
      dirs <- projectDirsA
      let path = uriPath uri
      fileExists <- doesFileExist path
      if fileExists
        then do
          need [path]
          let resource = resourcePaths dirs base uri
          p <- publicResourceA
          withResource p 1 $
            liftIO $
            case method of
              Copy -> copyResource resource
              SymLink -> linkResource resource
              Absolute -> absRefResource resource
              Relative -> relRefResource base resource
        else throw $ ResourceException $ "resource does not exist: " ++ path

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
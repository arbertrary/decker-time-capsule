{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Resources
  ( extractResources
  , getResourceString
  , deckerResourceDir
  , writeExampleProject
  , writeResourceFiles
  , copyDir
  ) where

import Common
import Control.Exception
import Control.Monad
import Control.Monad.Extra
import Data.ByteString.Lazy as BS (readFile)
import Exception
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process

deckerResourceDir :: IO FilePath
deckerResourceDir =
  getXdgDirectory
    XdgData
    ("decker" ++ "-" ++ deckerVersion ++ "-" ++ deckerGitBranch)

getResourceString :: FilePath -> IO String
getResourceString path = do
  dataDir <- deckerResourceDir
  Prelude.readFile (dataDir </> path)

-- Extract resources from the executable into the XDG data directory.
extractResources :: IO ()
extractResources = do
  deckerExecutable <- getExecutablePath
  dataDir <- deckerResourceDir
  exists <- doesDirectoryExist dataDir
  unless exists $ do
    unlessM (Resources.unzip ["-l", deckerExecutable]) $
      throw $ ResourceException "No resource zip found in decker executable."
    createDirectoryIfMissing True dataDir
    unlessM (Resources.unzip ["-qq", "-o", "-d", dataDir, deckerExecutable]) $
      throw $
      ResourceException "Unable to extract resources from decker executable"
    putStrLn $ "# resources extracted to " ++ dataDir

unzip :: [String] -> IO Bool
unzip args = do
  (exitCode, _, _) <- readProcessWithExitCode "unzip" args ""
  return $
    case exitCode of
      ExitSuccess -> True
      ExitFailure 1 -> True
      _ -> False

writeExampleProject :: IO ()
writeExampleProject = writeResourceFiles "example" "."

writeResourceFiles :: FilePath -> FilePath -> IO ()
writeResourceFiles prefix destDir = do
  dataDir <- deckerResourceDir
  let src = dataDir </> prefix
  copyDir src destDir
  -- exists <- doesDirectoryExist (destDir </> prefix)
  -- unless exists $ copyDir src destDir

-- Check difference between file contents
diff :: FilePath -> FilePath -> IO Bool
diff src dst = do
  srcContents <- BS.readFile src
  destContents <- BS.readFile dst
  return (srcContents == destContents)

-- Copy a file to a file location or to a directory
cp :: FilePath -> FilePath -> IO ()
cp src dst = do
  unlessM (doesFileExist src) $
    throw (userError "src does not exist or is not a file")
  dstExists <- doesFileExist dst
  if not dstExists
    then do
      destIsDir <- doesDirectoryExist dst
      if destIsDir
        then copyFile src (dst </> takeFileName src)
        else copyFile src dst
    else unlessM (diff src dst) $ copyFile src dst

-- Copy a directory and its contents recursively
copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
  unlessM (doesDirectoryExist src) $
    throw (userError "src does not exist or is not a directory")
  dstExists <- doesDirectoryExist dst
  if dstExists && (last (splitPath src) /= last (splitPath dst))
    then copyDir src (dst </> last (splitPath src))
    else do
      createDirectoryIfMissing True dst
      contents <- getDirectoryContents src
      let xs = Prelude.filter (`notElem` [".", ".."]) contents
      forM_ xs $ \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name
        isDirectory <- doesDirectoryExist srcPath
        if isDirectory
          then copyDir srcPath dstPath
          else cp srcPath dstPath

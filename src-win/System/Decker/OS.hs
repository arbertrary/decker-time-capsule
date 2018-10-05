{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de.de> --}
{-# LANGUAGE CPP #-}

module System.Decker.OS
  ( deckerResourceDir
  , fileLink
  , urlPath
  ) where

import Common
import Data.List
import Debug.Trace
import System.Directory
import System.Environment
import System.FilePath

deckerResourceDir :: IO FilePath
deckerResourceDir = do
  exep <- getExecutablePath
  pwd <- getCurrentDirectory
  let resDir =
        if isDebug
          then pwd </> "resource"
          else joinPath [(takeDirectory exep), "..", "resource"]
  return resDir

fileLink :: FilePath -> FilePath -> IO ()
fileLink target link = copyRecursive target link

copyRecursive :: FilePath -> FilePath -> IO ()
copyRecursive src dest = do
  isDir <- doesDirectoryExist src
  if isDir
    then do
      exists <- doesDirectoryExist dest
      if exists
        then return ()
        else do
          contents <- listDirectory src
          createDirectory dest
          mapM (\x -> copyRecursive (src </> x) (dest </> x)) contents
          return ()
    else do
      exists <- doesFileExist dest
      if exists
        then do
          return ()
        else copyFileWithMetadata src dest

isDebug :: Bool
#ifdef DEBUG
isDebug = True
#else
isDebug = False
#endif
urlPath :: FilePath -> FilePath
urlPath path = intercalate "/" (splitDirectories path)

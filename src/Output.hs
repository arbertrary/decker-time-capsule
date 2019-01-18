-- | This module gathers all functions that output something during runtime
module Output
  ( getResourceString
  ) where

import Project

import System.FilePath

-- CLEANUP: From Resources
getResourceString :: FilePath -> IO String
getResourceString path = do
  dataDir <- deckerResourceDir
  readFile (dataDir </> path)
